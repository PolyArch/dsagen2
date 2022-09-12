package dsagen2.comp.module

import Chisel.DecoupledIO
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import dsagen2.comp.bundle._
import dsagen2.comp.config.CompKeys.{CompNode, DsaOperations, MetaControl, RegFile}
import dsagen2.comp.config.processing_element._
import dsagen2.comp.config.reconf.CompReconfEdge
import dsagen2.comp.config.{CompNodeParameters, WithCompNode}
import dsagen2.comp.diplomacy.CompDirEdgeParameters
import dsagen2.comp.impl._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.misc.module.MaskAggregator
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.config.DebugPrintable
import dsagen2.top.config.enumeration.EnumEncodeMethod
import dsagen2.top.config.operation.Operation._
import dsagen2.top.diplomacy.DSANodeType.ProcessingElement
import dsagen2.util.StreamUtil.mergeVecData
import dsagen2.util.{QueueFPGA, WithQueueIO}

import scala.collection.mutable.ListBuffer
import scala.math._

/** * Processing Element Implementation
  * {{{
  * Inputs -> MUXs -> FIFOs -> ALU -> Control-> Buffers -> Outputs
  *        ^             |            ^    v
  *        |              -----ctrl----    |
  *        ----------Register File -------
  *
  * --X-->: means any to any connection
  * -->   : means one to one connection
  *
  * MUXs: combinational, zero cycle delay
  *  for operands routing: (#register + #input) * compBits --X--> #operands * compBits
  *  for control routing: #input * compBits[keyBits - 1, 0] --X--> 1 * keyBits
  *
  * FIFOs: stated, multiple cycles delay
  *  for operands delay: #operand * compBits --> #operand * compBits
  *    #operands delay fifo (static / dynamic)
  *  for control delay: 1 * keyBits --> 1 * keyBits
  *    1 delay, just standard queue
  *
  * ALU: combinational, mandatory since reuse signal needs to be calculated
  *  input: #operand * compBits,
  *          dequeue.ready = control's passed back ready from buffers & !reuse from control if meta control enabled
  *              (only take #result that belongs to current opcode)
  *  output: #result * compBits
  *
  * Control: A simple lookup table that either select input or use same cycle ALU result as key to control
  *  for data part:
  *    input: #result * compBits
  *    output: #result * compBits --> #result output buffers, pass back buffers' ready
  *            #result * compBits --> #result register file write ports
  *  for control part:
  *    input: 1 * keyBits <- Mux(outputBits[0][keyBits - 1, 0], controlInput[keyBits - 1,0])
  *    output: control signal
  *      reset: mask to the register file, reset the corresponding resettable register
  *      reuse: mask to ALU, and turn off dequeue signal
  *      discard: mask to turn off the result by compBits elements
  *      abstain (do nothing): overwrite reuse to be all true, reset to be all false, discard to be all true.
  *
  * Buffers: #result standard FIFO queue
  *    input: #result * compBits
  *    output: #result * compBits
  * }}}
  *
  * @param compNode  Compute Node Parameters
  * @param inParams  Input Bundle Parameters
  * @param outParams Output Bundle Parameters
  * @param p         CDE parameters tree
  */
class ProcessingElementImpl(
  val compNode:    CompNodeParameters,
  val reconfParam: CompReconfEdge,
  val inParams:    Seq[CompDirEdgeParameters],
  val outParams:   Seq[CompDirEdgeParameters]
)(
  implicit val p: Parameters)
    extends CompModuleImpl
    with NDelayFIFOImpl[CompDataBundle]
    with RegisterFileImpl
    with MetaControlImpl
    with AluModuleImpl
    with NBufferImpl[CompDataBundle]
    with DebugPrintable {

  /* -------------------------      Extract Parameters      ------------------------- */

  // Extract the parameter
  lazy val aluParam:  PEDsaOperationParameters = p(DsaOperations) // must
  lazy val regParam:  Option[PERegFileParameters] = p.lift(RegFile) // option
  lazy val ctrlParam: Option[PEMetaCtrlParameters] = p.lift(MetaControl) // option

  // Config Update
  val updateConfig :: updateRegister :: Nil = Enum(2)

  /* -------------------------       Parameters Setup       ------------------------- */

  // Number of register read = #maxOperand
  def numRegRead: Int = maxNumOperand

  def numRegWrite: Int = maxNumResult

  /* -------------------------       Diplomatic Node        ------------------------- */
  /* -------------------------    Parameters Sanity Check   ------------------------- */
  /* -------------------------        Input / Output        ------------------------- */
  /* -------------------------    Registers Instantiation   ------------------------- */

  // Bitstream Register for Holding the configuration
  val configStateReg: Vec[UInt] = RegInit(VecInit(Seq.fill(cfgGroup)(0.U(CONF_CFG_DATA_BITS.W))))

  // Register to keep track of the current selected instruction
  val currSelInst: Option[UInt] = if (instSlotSize > 1) Some(RegInit(0.U(log2Ceil(instSlotSize).W))) else None

  // Register to keep track of number of instruction on the flight
  val numFlightInst: UInt = RegInit(0.U(log2Ceil(bufferDepth + 1).W))

  /* -------------------------     Modules Instantiation    ------------------------- */

  // Queue to buffer the input control
  val inputCtrlQueue: Option[WithQueueIO[UInt]] =
    if (supportInputCtrl)
      Some(Module(new QueueFPGA[UInt](UInt(ctrlParam.get.keyBits.W), aluParam.maxFifoDepth, p(FPGAOverlay))))
    else None

  /* -------------------------    Wires Create/Connection   ------------------------- */

  // Decode Config Bitstream
  val configBits: PECfgBits = configStateReg.asTypeOf(new PECfgBits(inParams.length, outParams.length))

  // Picking up the current selected operation
  val currInst: PEFuncUnitInst = Wire(new PEFuncUnitInst(numInput, numOutput))

  // Current selected instruction is valid
  val currInstValid: Bool = currInst.valid

  // Extract the ALU setting (instruction slot from config bits stream)
  val fuInsts: Vec[PEFuncUnitInst] = configBits.instSlot

  // Meta Control Input
  val ctrlKey: Option[ValidIO[UInt]] =
    if (supportCtrl) Some(Wire(ValidIO(UInt(ctrlParam.get.keyBits.W)))) else None

  // Extract the Meta Control Stream Table from the config bits stream
  val metaLUT: Option[Vec[PECtrlEntry]] = configBits.ctrlLUT

  // Meta Control Output
  val regReset:  Option[UInt] = if (supportResetReg) Some(Wire(UInt(regParam.get.numResetReg.W))) else None
  val operReuse: Option[UInt] = if (supportReuseOper) Some(Wire(UInt(aluParam.maxNumOperand.W))) else None
  val resultDis: Option[UInt] = if (supportDiscard) Some(Wire(UInt(aluParam.maxNumResult.W))) else None
  val abstain:   Option[Bool] = if (supportAbstain) Some(Wire(Bool())) else None

  /* Actual IO For ALU */
  val aluOperands: Vec[DecoupledIO[UInt]] =
    Wire(Vec(maxNumOperand, DecoupledIO(UInt(dataPathBits.W))))
  val aluOpcode: Option[UInt] =
    if (opDataTypeSet.size > 1)
      Some(WireDefault(0.U(log2Ceil(opDataTypeSet.size).W)))
    else None
  val aluResults: Vec[DecoupledIO[UInt]] = Wire(Vec(maxNumResult, DecoupledIO(UInt(dataPathBits.W))))
  // TODO: we don't have a plan to deal with exception in ALU for now
  val aluExceptions: EnumBundle = WireDefault(
    0.U.asTypeOf(
      new EnumBundle(
        exceptionSet, // Define the operation exception set
        EnumEncodeMethod.maskEncode, // I think the computation exception should be one hot
        supportInvalid = false, // If it is the union set of all exceptions, why should we support invalid state?
        isOneHot = true // I think it is one hot, let see what will happen
      )
    )
  )

  // Operands selected from input ports and optional register read wire
  val operands: Seq[DecoupledIO[CompDataBundle]] =
    Seq.fill(maxNumOperand)(WireInit(0.U.asTypeOf(Decoupled(new CompDataBundle(compNode)))))

  // Operands selection extracted from bit stream
  val operandsSel: Seq[UInt] = currInst.operSels.getOrElse(Seq.fill(maxNumOperand)(0.U))

  // Results selection for output ports
  val resOutSel: Seq[UInt] = currInst.resOutSel.getOrElse(Seq.fill(maxNumResult)(0.U))

  // Result selection for register write
  val resRegSel: Seq[UInt] = currInst.resRegSel.getOrElse(Seq.fill(maxNumResult)(0.U))

  // Result selection for register write

  // Optional input controlled signal selected
  val inputCtrlEnqueue: Option[DecoupledIO[UInt]] =
    if (supportInputCtrl) Some(inputCtrlQueue.get.io.enq) else None

  // Optional Controlled Input Selected from Input Ports buffered by queue
  val inputCtrlDequeue: Option[DecoupledIO[UInt]] =
    if (supportInputCtrl) Some(inputCtrlQueue.get.io.deq) else None

  // Source selection for input controlled
  val inputCtrlSel: UInt = currInst.inputCtrlSel.getOrElse(0.U)

  // Delay Buffer Input and outputs
  val delayFifosIn: Seq[DecoupledIO[CompDataBundle]] = operands
  val delayFifosOut: Seq[DecoupledIO[CompDataBundle]] =
    Seq.fill(maxNumOperand)(WireInit(0.U.asTypeOf(Decoupled(new CompDataBundle(compNode)))))
  val delayFifosCount: Seq[UInt] = currInst.delayCount match {
    case Some(counts) => counts
    case None         => Seq.fill(maxNumOperand)(WireInit(0.U))
  }
  val delayFifosBusy: Seq[Bool] = Seq.fill(maxNumOperand)(WireInit(false.B))

  // Buffer the results from ALU, to be sent to De-mux for output ports
  val buffersInput: Seq[DecoupledIO[CompDataBundle]] =
    Seq.fill(maxNumResult)(Wire(DecoupledIO(new CompDataBundle(compNode))))
  val buffersOutput: Seq[DecoupledIO[CompDataBundle]] =
    Seq.fill(maxNumResult)(Wire(DecoupledIO(new CompDataBundle(compNode))))
  val buffersCount: Seq[UInt] = Seq.fill(maxNumResult)(Wire(UInt(depthBits.W)))

  // Input control key valid, instruction is valid, dequeue key is valid, control mode is input control
  val inputCtrlKeyValid: Option[Bool] =
    if (supportInputCtrl)
      Some(
        currInstValid &&
          inputCtrlDequeue.get.valid && currInst.ctrlMode.getOrElse(noCtrl) === inputCtrl
      )
    else None

  // Input control key bits
  val inputCtrlKeyBits: Option[UInt] =
    if (supportInputCtrl) {
      // Apply bit aggregation by using BMSS
      val (aggBits, _) = MaskAggregator(inputCtrlDequeue.get.bits.asBools(), configBits.bmss.get.asBools())
      Some(VecInit(aggBits).asUInt())
    } else None

  // Output control key bits, use the first result lower key bits
  val outputCtrlBits: Option[UInt] =
    if (supportOutputCtrl) Some(aluResults.head.bits.apply(ctrlParam.get.keyBits - 1, 0)) else None

  // Output control key is valid: first output results is valid, instruction is valid
  val outputCtrlKeyValid: Option[Bool] =
    if (supportOutputCtrl)
      Some(
        currInstValid &&
          aluResults.head.valid && currInst.ctrlMode.getOrElse(noCtrl) === outputCtrl
      )
    else None

  // ALU result valid combines with optional discard from control
  val aluResultsValid: Seq[Bool] = aluResults.zipWithIndex.map { case (aluResult, resultIdx) =>
    if (resultDis.isDefined) require(resultDis.get.getWidth == maxNumResult)
    // Valid result means valid from ALU and not discarded
    aluResult.valid && !resultDis.getOrElse(0.U(maxNumResult.W)).asBools().apply(resultIdx)
  }

  // Read the register file, inward direction, valid is read enable
  val regReadsValid: Option[Vec[Bool]] = // Read Enable
    if (supportReg) Some(WireInit(VecInit(Seq.fill(numRegRead)(false.B)))) else None
  val regReadsIdx: Option[Vec[UInt]] = // Read Register Index
    if (supportReg && regIdxBits > 0) Some(WireInit(VecInit(Seq.fill(numRegRead)(0.U(regIdxBits.W))))) else None
  val regReadsData: Option[Vec[ValidIO[UInt]]] = // Register Output Valid
    if (supportReg) Some(WireInit(VecInit(Seq.fill(numRegRead)(0.U.asTypeOf(ValidIO(UInt(regBits.W))))))) else None

  // Write to the register file, inward direction, valid is write enable, bits is register number
  val regWritesValid: Option[Vec[Bool]] =
    if (supportReg) Some(WireInit(VecInit(Seq.fill(numRegWrite)(false.B)))) else None
  val regWritesIdx: Option[Vec[UInt]] =
    if (supportReg && regIdxBits > 0) {
      Some(WireInit(VecInit(Seq.fill(numRegWrite)(0.U(regIdxBits.W)))))
    } else None
  val regWritesData: Option[Vec[UInt]] =
    if (supportReg) Some(WireInit(VecInit(Seq.fill(numRegWrite)(0.U(regBits.W))))) else None

  // Register File Update Port, since every time we can only write up to 48 bits from config port
  // update to the register file is done in the granularity of physical register
  val regUpdValid: Option[Bool] =
    if (supportUpdateRegister) Some(WireInit(false.B)) else None
  val regUpdIdx: Option[UInt] =
    if (supportUpdateRegister && phyRegIdxBits > 0)
      Some(WireInit(0.U(phyRegIdxBits.W)))
    else None
  val regUpdData: Option[UInt] =
    if (supportUpdateRegister) Some(WireInit(0.U(CONF_RF_MAX_BITS.min(regBits).W))) else None

  // Reset the Register File when PE is configured
  val resetRF: Option[Bool] = if (supportReg) Some(WireInit(false.B)) else None

  // Register Read Wire Bits, length is the maxNumOperand
  val regReadVecData: Option[Seq[ValidIO[CompDataBundle]]] = regReadsData match {
    case Some(regReadPorts) =>
      require(
        regReadPorts.length == maxNumOperand,
        s"register read port = ${regReadPorts.length}," +
          s"max number of operands = $maxNumOperand"
      )
      val result: Seq[ValidIO[CompDataBundle]] =
        regReadPorts.map { readPort: ValidIO[UInt] =>
          val readDataWithPred: ValidIO[CompDataBundle] = Wire(ValidIO(new CompDataBundle(compNode)))
          // Connect Data Bits
          val readData: UInt = readPort.bits
          require(readData.getWidth == readDataWithPred.bits.vecData.map(_.getWidth).sum)
          require(readDataWithPred.bits.vecData.forall(_.getWidth == compNode.compUnitBits))
          readDataWithPred.bits.vecData.zipWithIndex.foreach { case (data, idx) =>
            data := readData(compNode.compUnitBits * (idx + 1) - 1, compNode.compUnitBits * idx)
          }
          // Connect Valid
          readDataWithPred.valid := readPort.valid
          // Return
          readDataWithPred
        }
      Some(result)
    case None => None
  }

  // ALU Fired based on opcode
  val aluFired: Bool = {
    // Calculate the result for different kinds of numOperands
    val firedEachNumOperand: Map[Int, Bool] = {
      for (numOper <- 1 to maxNumOperand) yield {
        val firedSignals: Seq[UInt] = {
          for (operIdx <- 0 until numOper) yield {
            delayFifosOut(operIdx).valid && aluOperands(operIdx).ready
          }
        }
        numOper -> VecInit(firedSignals).asUInt().andR()
      }
    }.toMap
    // Generate Opcode to Operation Fired signal based on the number of operand required by this opcode
    val opcode2fired: Seq[(UInt, Bool)] = aluParam.getDsaOpDataType2EncMap.map { case (opDT, encoding) =>
      encoding.U -> firedEachNumOperand(opDT.operation.numOperand)
    }.toSeq
    MuxLookup(aluOpcode.getOrElse(0.U), false.B, opcode2fired)
  }

  // Number of instruction fired
  val numFiredInst: UInt = numFlightInst + buffersCount.head

  // Onflight Inst: Whether or not can be fired based on buffer size, taking the on-flight instruction into account
  // Input control key valid: either not input controlled or input control key is valid
  val canFire: Bool = numFiredInst < bufferDepth.U &&
    (currInst.ctrlMode.getOrElse(noCtrl) =/= inputCtrl || inputCtrlKeyValid.getOrElse(true.B))

  /* -------------------------      Combination Logic       ------------------------- */

  // Connect the current selected instruction from instruction slot

  if (instSlotSize > 1) {
    // PE with multiple instructions
    require(currSelInst.isDefined)
    currInst := configBits.instSlot(currSelInst.get)
  } else {
    // PE with only one instruction
    currInst := configBits.instSlot.head
  }

  // Connect Delay Output to ALU
  require(
    aluOperands.length == delayFifosOut.length,
    s"ALU has ${aluOperands.length} operands, " +
      s"but you have ${delayFifosOut.length} fifos"
  )
  aluOperands.zip(delayFifosOut).zipWithIndex.foreach { case ((aluOper, delayOut), operIdx) =>
    // We do not use the predication of value
    // TODO: I don't think this will make much difference for now
    //  since all predicated off value is zero, it will not cause flip of value, which means no dynamic power
    //  is wasted. We should come back to this place when we have triggered instruction
    aluOper.bits := mergeVecData(delayOut.bits.vecData)
    aluOper.valid := delayOut.valid && canFire
    // Operand queue ready is combined with operand reuse
    // Dequeue the operands needs the ALU to be ready and not reuse
    delayOut.ready := canFire &&
      aluOper.ready && !operReuse.getOrElse(0.U(maxNumOperand.W)).asBools().apply(operIdx)
  }

  // Connect Opcode to ALU Opcode
  aluOpcode match {
    case Some(opcode) =>
      require(currInst.fuOpcode.isDefined)
      require(opcode.getWidth == currInst.fuOpcode.get.getWidth)
      opcode := currInst.fuOpcode.get
    case None =>
  }

  // Connect to the control key
  ctrlKey match {
    case Some(ctlkey) =>
      // control key valid is or of both control mode
      ctlkey.valid := inputCtrlKeyValid.getOrElse(false.B) || outputCtrlKeyValid.getOrElse(false.B)
      // control key bits is selected between input control bits and output control bits
      ctlkey.bits := Mux(
        inputCtrlKeyValid.getOrElse(false.B),
        inputCtrlKeyBits.getOrElse(0.U),
        Mux(outputCtrlKeyValid.getOrElse(false.B), outputCtrlBits.getOrElse(0.U), 0.U)
      )
    case None =>
  }

  // Connect optional [[regReadsData]] and [[compInPorts]] to [[operands]] by using [[operandsSel]]
  // Connect Bits and Valid
  for (operIdx <- 0 until maxNumOperand) {
    val sel: UInt = operandsSel(operIdx)
    // Generate Bits Lookup
    val operandBitsLUT: Seq[(UInt, CompDataBundle)] =
      for (selIdx <- 0 until 1 + numInput + numReg) yield {
        if (selIdx == 0) {
          selIdx.U -> 0.U.asTypeOf(new CompDataBundle(compNode))
        } else if (selIdx > 0 && selIdx < 1 + numInput) {
          selIdx.U -> compInPorts(selIdx - 1).data.get
        } else {
          require(regReadVecData.isDefined, s"If you do not have register, why you end up here")
          selIdx.U -> regReadVecData.get.apply(operIdx).bits
        }
      }
    // Generate Valid Lookup
    val operandValidLookup: Seq[(UInt, Bool)] = {
      for (selIdx <- 0 until 1 + numInput + numReg) yield {
        if (selIdx == 0) {
          selIdx.U -> false.B
        } else if (selIdx > 0 && selIdx < 1 + numInput) {
          selIdx.U -> compInPorts(selIdx - 1).valid.get
        } else {
          selIdx.U -> regReadsData.get.apply(operIdx).valid
        }
      }
    }
    operands(operIdx).bits := MuxLookup(sel, 0.U.asTypeOf(new CompDataBundle(compNode)), operandBitsLUT)
    operands(operIdx).valid := MuxLookup(sel, false.B, operandValidLookup)
  }
  // Connect Ready of Compute Input Ports
  for (inputIdx <- 0 until numInput) {
    compInPorts(inputIdx).ready.get := false.B // by default, we assume no operands use this input port
    for (operIdx <- 0 until maxNumOperand) {
      val sel: UInt = operandsSel(operIdx)
      // If select line of the operand pick this one, then use it ready
      // when in loop will create a sequential mux
      when(sel === (inputIdx + 1).U) {
        compInPorts(inputIdx).ready.get := operands(operIdx).ready
      }
    }
  }

  // Connect [[operandsSel]] to [[regReadsValid]] and [[regReadsIdx]]
  if (supportReg) {
    for (operIdx <- 0 until maxNumOperand) {
      val sel: UInt = operandsSel(operIdx)
      // Get the valid bit for other operand
      val otherAluOperValids: Seq[Bool] = aluOperands.zipWithIndex.filter { case (_, aluOperIdx) =>
        aluOperIdx != operIdx
      }.map(x => x._1.valid && !x._1.ready)
      // Since read from the register as small latency (up to 1), we read register file when other operands are valid
      regReadsValid.get.apply(operIdx) := VecInit(otherAluOperValids).asUInt().andR() && sel >= (1 + numInput).U
      regReadsIdx match {
        case Some(regIdx) => regIdx(operIdx) := (sel - (1 + numInput).U)
        case None         =>
      }
    }
  }

  // Connect input ports to [[inputCtrlEnqueue]] by using [[inputCtrlSel]]
  if (supportInputCtrl) {
    // Connect Bits and Valid
    val inputCtrlLUT: Seq[(UInt, (UInt, Bool))] = for (selIdx <- 0 until 1 + numInput) yield {
      if (selIdx == 0) {
        selIdx.U -> (0.U, false.B)
      } else {
        selIdx.U -> (mergeVecData(compInPorts(selIdx - 1).data.get.vecData)
          .apply(ctrlParam.get.keyBits - 1, 0), compInPorts(selIdx - 1).valid.get)
      }
    }
    val bitsLUT:  Seq[(UInt, UInt)] = inputCtrlLUT.map(x => x._1 -> x._2._1)
    val validLUT: Seq[(UInt, Bool)] = inputCtrlLUT.map(x => x._1 -> x._2._2)
    inputCtrlEnqueue.get.bits := MuxLookup(inputCtrlSel, 0.U, bitsLUT)
    inputCtrlEnqueue.get.valid := MuxLookup(inputCtrlSel, false.B, validLUT)
    // Connect Ready
    for (inputIdx <- 0 until numInput) {
      when(inputCtrlSel === (inputIdx + 1).U) {
        compInPorts(inputIdx).ready.get := inputCtrlEnqueue.get.ready
      }
    }
  }

  // Controlled input is dequeued by the instruction fired
  if (supportInputCtrl) {
    inputCtrlDequeue.get.ready := aluResults.head.valid
  }

  // Connect the ALU result bits/valid/ready to [[buffersInput]]
  for (resultIdx <- 0 until maxNumResult) {
    // Connect Valid
    buffersInput(resultIdx).valid := aluResultsValid(resultIdx)
    // Connect Data
    for (compUnitIdx <- 0 until compNode.numCompUnit) {
      // Connect TagValue
      buffersInput(resultIdx).bits.vecData(compUnitIdx) := aluResults(resultIdx)
        .bits(compNode.compUnitBits * (compUnitIdx + 1) - 1, compNode.compUnitBits * compUnitIdx)
    }
    // Connect Ready
    aluResults(resultIdx).ready := buffersInput(resultIdx).ready
  }

  // Connect the ALU result bits/valid to [[regWritesData]], overwritten by reset, using [[resRegSel]]
  if (supportReg) {
    for (resultIdx <- 0 until maxNumResult) {
      val reset = regReset match {
        case Some(r) => if (resultIdx < r.getWidth) r.asBools().apply(resultIdx) else false.B
        case None    => false.B
      }
      // Overwrite register write by reset
      when(reset) {
        // Reset is trigger by result valid still
        regWritesValid.get.apply(resultIdx) := {
          if (resultIdx < regParam.get.numResetReg) aluResults(resultIdx).valid else false.B
        }
        // Use the nth register write port to update the nth resettable register
        regWritesIdx match {
          case Some(idxes) =>
            idxes(resultIdx) := {
              if (resultIdx < regParam.get.numResetReg) regParam.get.resetRegIdx(resultIdx).U
              else 0.U
            }
          case None =>
        }
        // Use reset register is zero
        regWritesData.get.apply(resultIdx) := 0.U
      }.otherwise {
        // Result from ALU is valid, select reg is larger than 0, 0 means do not write
        // The reason that not using [[aluResultsValid(resultIdx)]] is that
        // this result will be turn off by discard, but discard is for output port, not for register write
        regWritesValid.get.apply(resultIdx) := aluResults(resultIdx).valid && resRegSel(resultIdx) > 0.U
        regWritesIdx match {
          case Some(writesIdx) => writesIdx(resultIdx) := resRegSel(resultIdx) - 1.U
          case None            =>
        }
        require(
          regWritesData.get.apply(resultIdx).getWidth == aluResults(resultIdx).bits.getWidth,
          s"register write port bit width = ${regWritesData.get.apply(resultIdx).getWidth}, " +
            s"alu result bit width = ${aluResults(resultIdx).bits.getWidth}"
        )
        regWritesData.get.apply(resultIdx) := aluResults(resultIdx).bits
      }
    }
  }

  // Connect the [[buffersOutput]] to [[compOutPorts]] by using [[resOutSel]]
  // Bits and Valid
  for (outputIdx <- 0 until numOutput) {
    val oneHot: Seq[Bool] = resOutSel.map(s => s === (outputIdx + 1).U)
    compOutPorts(outputIdx).data.get := Mux1H(oneHot, buffersOutput.map(_.bits))
    compOutPorts(outputIdx).valid.get := Mux1H(oneHot, buffersOutput.map(_.valid))
  }
  // Ready
  for (resIdx <- 0 until maxNumResult) {
    val oneHot: Seq[Bool] = for (outputIdx <- 1 to numOutput) yield {
      resOutSel(resIdx) === outputIdx.U
    }
    buffersOutput(resIdx).ready := Mux1H(oneHot, compOutPorts.map(_.ready.get))
  }

  // Connect the [[delayFifosCount]] and [[buffersCount]] to [[compNodeStatus]]
  val dataInOutputBuffer: Bool = VecInit(buffersCount.map(_ > 0.U)).asUInt().orR()
  val dataInDelayBuffer:  Bool = VecInit(delayFifosBusy).asUInt().orR()
  compStatus.busy := configBits.enabled && (dataInOutputBuffer || dataInDelayBuffer)

  // Connect the config bits aliveness to status port
  compStatus.alive := configBits.enabled

  // Connect instruction firing status to status port
  compStatus.fired := aluFired

  // Report the hardware type of PE to status port
  compStatus.hwType := peCompType

  /* ---------- Final Construction for each component ---------- */
  // Delay the operands
  delay()
  // Compute the results
  compute()
  // Control
  control()
  // Register File
  registerFile()
  // Buffer output
  buffers()

  /* -------------------------     Finite State Machine     ------------------------- */

  // Update the config state register
  val configThis: Bool =
    configPort.valid && configPort.nodeId === compNode.getNodeId.U && configPort.nodeType === peConfType
  when(configThis) {
    when(configPort.cfgGroup === updateConfig) {
      // update the ALU + Meta Control Config
      configStateReg(configPort.cfgIndex) := configPort.cfgBits
    }
    // 11 cfg group means update the register file
    if (supportUpdateRegister) {
      when(configPort.cfgGroup === updateRegister) {
        regUpdValid.get := true.B
        regUpdIdx match {
          case Some(value) => value := configPort.cfgIndex
          case None        =>
        }
        regUpdData.get := configPort.cfgBits
      }
    }
  }

  // Reset the register
  resetRF match {
    case Some(value) => value := configThis;
    case None        =>
  }

  // Update the number of flying instruction
  when(aluFired && aluResults.head.fire()) {
    // Do nothing to number of instruction counter
  }.elsewhen(aluFired) {
    numFlightInst := numFlightInst + 1.U
  }.elsewhen(aluResults.head.fire()) {
    numFlightInst := numFlightInst - 1.U
  }

  /* -------------------------      Output Connection       ------------------------- */

  // Report Node Statue to other nodes
  compOutPorts.foreach { output =>
    output.ctrl.get.dAct match {
      case Some(bool) => bool := RegNext(configBits.enabled);
      case None       =>
    }
  }

  compInPorts.foreach { input =>
    input.ctrl.get.uAct match {
      case Some(bool) => bool := RegNext(configBits.enabled);
      case None       =>
    }
  }

  // Connect the config status
  compStatus.configured := configThis

  /* -------------------------     Hardware Sanity Check    ------------------------- */
  /* -------------------------       Utility Function       ------------------------- */
  /* -------------------------      Derived Parameters      ------------------------- */

  // How many bits need to control this node
  // Only ALU and Meta Control Info
  def csrBits: Int = 1 + aluParam.csrBits(inParams.length, outParams.length) + {
    ctrlParam match {
      case Some(param) => param.csrBits
      case None        => 0
    }
  }

  // How many config group needed
  def cfgGroup: Int = csrBits / CONF_CFG_DATA_BITS + {
    if (csrBits % CONF_CFG_DATA_BITS == 0) 0 else 1
  }

  // Number of instruction supported per configuration
  def instSlotSize: Int = aluParam.instSlotSize

  /* ------------------------- Post Generation Sanity Check ------------------------- */

  // Check the length of operands selection is same with max number of operands
  require(
    operandsSel.length == maxNumOperand,
    s"You have ${operandsSel.length} to select the source for operand, but you only have $maxNumOperand operands"
  )

  // Make sure that the input/output/operand/result have all same data width
  require(aluOperands.forall(x => x.bits.getWidth == compNode.compBits))
  require(aluResults.forall(x => x.bits.getWidth == compNode.compBits))
  compInPorts.foreach { in =>
    require(in.data.isDefined)
    require(in.data.get.vecData.getWidth == compNode.compBits)
  }
  compOutPorts.foreach { out =>
    require(out.data.isDefined)
    require(out.data.get.vecData.getWidth == compNode.compBits)
  }

  /* ------------------------- Utility For Debug Purpose ------------------------- */
  if (printDebug) {
    println(s"${compNode.getNodeName}'s Instructions : ")
    aluParam.getDsaOpDataType2EncMap.foreach { case (opDT, encoding) =>
      println(s"$opDT\t:\t$encoding")
    }
  }
}

object ProcessingElementImpl extends App {

  var samples = ListBuffer[(Int, Int, Int, Boolean, Int, Int, Int, Int)]()
  println(s"Generating Values")

  for (input <- 2 until 16) {
    for (output <- 1 until 16) {
      for (outputBufferDepth <- 1 until 10) {
        for (static <- Seq(true, false)) {
          for (fifoDepth <- 1 until 16) {
            for (numReg <- 1 until 10) {
              for (datawidth <- Seq(8, 16, 32, 64, 128)) {
                for (granularity <- Seq(8, 16, 32, 64)) {
                  if (granularity <= datawidth) {
                    samples += ((input, output, outputBufferDepth, static, fifoDepth, numReg, datawidth, granularity))
                  }
                } // End of explore granularity
              } // End of explore datawidth
            } // End of explore number of register
          } // End of explore FIFO depth
        } // End of explore dynamic / static
      } // End of explore output buffer
    } // End of explore number of output
  } // End of explore number of input

  val samplesList = scala.util.Random.shuffle(samples.toList)

  println(s"Total number of configurations: ${samplesList.length}")
  println(s"Start to generate the configurations")

  val iter_number = min(100000, samplesList.length)

  for (n <- 0 to iter_number) {
    val (input, output, outputBufferDepth, static, fifoDepth, numReg, datawidth, granularity) = samplesList(n)
    println(
      s"Generating configuration $n/$iter_number: input=$input, output=$output, outputBufferDepth=$outputBufferDepth, static=$static, fifoDepth=$fifoDepth, numReg=$numReg, datawidth=$datawidth, granularity=$granularity"
    )

    val cdeParameter: Parameters =
      new WithRegister(numReg) ++
        new WithMetaControl() ++
        new WithDsaOperations(
          Set(FixedAdd),
          1,
          fifoDepth,
          0,
          0,
          true
        ) ++
        new WithCompNode(
          CompNodeParameters(
            nodeType = ProcessingElement,
            nodeId = 0,
            compBits = datawidth,
            compUnitBits = granularity
          ),
          outputBufferDepth,
          static
        )

    // Extract parameter from CDE
    val compNode:    CompNodeParameters = cdeParameter(CompNode)
    val reconfParam: CompReconfEdge = CompReconfEdge(p = cdeParameter)

    // Generate Input / Output Parameters
    val inParams: Seq[CompDirEdgeParameters] =
      Seq.fill(input)(new CompDirEdgeParameters(true, compNode, cdeParameter))
    val outParams: Seq[CompDirEdgeParameters] =
      Seq.fill(output)(new CompDirEdgeParameters(true, compNode, cdeParameter))

    // Generate Verilog
    (new ChiselStage).emitVerilog(
      new ProcessingElementImpl(compNode, reconfParam, inParams, outParams)(cdeParameter),
      Array(
        "--full-stacktrace",
        "--target-dir",
        "/data/dkupsh/processing_element/data/" + input + "-I_" + output + "-O_" + outputBufferDepth + "-OBD_" + static + "-S_" + fifoDepth + "-FD_" + numReg + "-R_" + datawidth + "-DW_" + granularity + "-G"
      )
    )
  }
}
