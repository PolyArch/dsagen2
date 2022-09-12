package dsagen2.sync.impl

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import dsagen2.comp.bundle.CompDirBundle
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.reconf.CompReconfEdge
import dsagen2.comp.diplomacy.CompDirEdgeParameters
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.{MemWriteBundle, StreamState}
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.OVP2MemParameter
import dsagen2.sync.bundle.{OVPSetPort, OVPWriteBus, VPRouteCfgBits, VectorPortStatus}
import dsagen2.sync.config.OVPNodeParameters
import dsagen2.sync.config.SyncKeys.OVPNode
import dsagen2.sync.module.AggDisMultiIOQueue
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.config.enumeration.VPImplMode._
import dsagen2.util.BooleanUtil.boolean2int
import dsagen2.util.StreamUtil._
import dsagen2.util.{QueueFPGA, WithQueueIO}
import freechips.rocketchip.tile.XLen

import scala.collection.mutable.ListBuffer
import scala.math._

/** The hardware implementation of output vector port
  *
  * @param ovpNode        Output Vector Port Node Parameter
  * @param reconfParam    Optional Reconfigurable Node Parameter
  * @param memWriteParams Memory Write Side Parameter
  * @param compParams     Compute Side Parameter
  * @param p              CDE parameter
  */
class OutputVectorPortImp(
  val ovpNode:        OVPNodeParameters,
  val reconfParam:    Option[CompReconfEdge],
  val memWriteParams: Seq[OVP2MemParameter],
  val compParams:     Seq[CompDirEdgeParameters]
)(
  implicit val p: Parameters)
    extends VectorPortImpl {

  /* ------------------------- Extract Parameters           ------------------------- */

  // Get the output vector port node Id, please be attention, this is NOT output vector port ID
  // This is the ID across all DSAGEN nodes, should only be used for identify DSA Nodes
  val vpNodeId:  Int = ovpNode.getNodeId
  val ovpNodeId: Int = vpNodeId
  val vpNodeTpe: UInt = ovpConfType

  // Compute Side XBar implementation
  val vpImpl: VPImpl = ovpNode.vpImpl

  // Get the depth of ovp
  val depth: Int = ovpNode.depthByte

  // Print debug info for output vector port
  if (printDebug)
    println(s"${compParams.length} Comp(s) --> ${ovpNode.getNodeName} --> ${memWriteParams.length} Mem(s)")

  // Suggest name for OVP
  suggestName(s"${ovpNode.getNodeName}_c${compParams.length}_m${memWriteParams.length}")

  /* ------------------------- Derived Parameters           ------------------------- */

  // Whether this output vector port support report state of stream by using the first compute port
  def supportState: Boolean = ovpNode.vpStated && memWriteParams.exists(_.memNode.streamStated)

  // Calculate the minimum compute port bits connect
  def compUnitBits: Int = compParams.drop(supportState).head.compNode.compBits

  // Calculate the width of compute in min compute bits, remove one for state penetration
  def compWidth: Int = compParams.length - supportState

  // Find the max memory band bit
  def maxMemBandBits: Int = memWriteParams.map(_.memNode.bandBits) max

  // Calculate the minimum memory unit connected
  def memUnitBits: Int = memWriteParams.head.memNode.memUnitBits

  // Calculate the memory write bus width
  def memWidth: Int = maxMemBandBits / memUnitBits

  // Calculate the minimum depth
  // Minimum: "2 *" is because of the concept of "double buffer", 1 for compute, 1 for memory
  // Middle:  "4 *" is because of the concept of "double buffer", 2 for compute, 2 for memory
  def minDepth: Int = (2 * memWidth * memUnitBits).max(2 * compWidth * compUnitBits) / memUnitBits

  // The actual depth used
  def finalDepth: Int = depth.max(minDepth)

  // Ratio of compute to memory unit
  def comp2mem: Int = compUnitBits / memUnitBits

  // Aggregate the data type of stream to get the maximum data type for all memory nodes connects to this OVP
  def maxDataTypeBits: Int = memWriteParams.map(_.memNode.memDataTypeBits).max

  // Aggregate the bits needed for Length1D across all memory node
  def maxL1DBits: Int = memWriteParams.map(_.memNode.length1DBits).max

  // Set up the number of queue input (compute side vector width)
  val isIVP:    Boolean = false
  val vecWidth: Int = boolean2int(supportState) + compWidth

  def supportPad: Boolean = false

  def padWidth: Int = -1

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // If stream state is supported, we must connect vp to more than two compute node, first one for state, rest for data
  if (supportState)
    require(compParams.length > 1, s"At least two compute ports required, but only ${compParams.length} is connected")

  // Huston, we have a problem
  require(
    compParams.nonEmpty && memWriteParams.nonEmpty,
    s"No compute/memory node parameters found in implementation, this is not good"
  )

  // Length 1D Bits cannot be zero
  require(maxL1DBits > 0, s"Length 1D Bits to enable keep enqueue to OVP should be positive, not $maxL1DBits")

  /* ------------------------- Input / Output               ------------------------- */

  // extract the memory write bundle
  val memWritePorts: Seq[MemWriteBundle] = memWriteParams.map(w => IO(new MemWriteBundle(w)))

  // extract the compute direction bundle
  val compPorts: Seq[CompDirBundle] = compParams.map(c => IO(Flipped(new CompDirBundle(c))))

  // Create input port for output vector port setting, TODO: Task Flow support
  val ovpSetPort: OVPSetPort = IO(Input(new OVPSetPort))

  // Create output port for reporting the output vector port status
  val ovpStatus: VectorPortStatus = IO(Output(new VectorPortStatus))

  /* ------------------------- Register                     ------------------------- */

  // Data Type of the stream that flowing through this output vector port
  // Updated for every usedByMem signal
  val memDataType: Option[UInt] = if (maxDataTypeBits > 0) Some(RegInit(0.U(maxDataTypeBits.W))) else None

  // Remaining length of current 1D stream, unit is not byte, unit should be calculated with register above
  // Update every new 1D stream, combined with the borrowed amount from next 1D stream
  val length1D: UInt = RegInit(0.U(maxL1DBits.W))

  /* ------------------------- Modules                      ------------------------- */

  // Multi-IO Queue
  val queue: AggDisMultiIOQueue = Module(
    new AggDisMultiIOQueue(
      numInput = compWidth,
      inputBits = compUnitBits,
      numOutput = memWidth,
      outputBits = memUnitBits,
      depthByte = finalDepth,
      inputXBarMode = ovpNode.vpImpl,
      outputXBarMode = NonXBarVP,
      this
    )
  )
  queue.repeatDequeue := false.B // never repeat at the output vector, why do we need it?

  // Queue to buffer stream state from the first compute node, a very shallow queue is enough
  val stateQueue: Option[WithQueueIO[StreamState]] =
    if (supportState) Some(Module(new QueueFPGA[StreamState](new StreamState, 4, p(FPGAOverlay)))) else None

  /* ------------------------- Wires                        ------------------------- */

  // C, Write Bus that aggregate the all write port of memory nodes
  val writeBus: OVPWriteBus = aggMemWritePorts

  // C, multi io queue dequeue fired
  val dequeueFire: Bool = queue.deqFire

  // Decode Vector Port Config Bitstream
  val configBits: Option[VPRouteCfgBits] =
    configStateReg match {
      case Some(value) => Some(value.asTypeOf(new VPRouteCfgBits(this)));
      case None        => None
    }

  // The exponential of number of stream unit per compute port, in unit of 2's power
  val strUnitExp: UInt = log2Ceil(comp2mem).U - memDataType.getOrElse(0.U)

  // The number of active lane is more than (>=) what needed for current remain Length1D
  val numActCompStrUnit: UInt = (queue.numActCompPort.get << strUnitExp).asUInt()
  queue.numNeedCompPort match {
    // Inform the output vector port inner queue that the right amount of element that can enqueue
    case Some(value) =>
      val tempNumCompPortNeed: UInt = (length1D >> strUnitExp).asUInt()
      value := Mux(tempNumCompPortNeed >= compWidth.U, compWidth.U, tempNumCompPortNeed)
    case None =>
  }
  val end1D:      Bool = numActCompStrUnit >= length1D
  val continue1D: Bool = !end1D

  /* ------------------------- Combination Logic            ------------------------- */

  // Connect the state port from inner queue that pass input side "XBar"
  if (supportState) {
    require(queue.statedPort.isDefined && stateQueue.isDefined, s"State Queue existence is problematic")
    stateQueue.get.io.enq.valid := queue.statedPort.get.valid // Enqueue Valid
    stateQueue.get.io.enq.bits := queue.statedPort.get.bits // Enqueue Bits
    queue.statedPort.get.ready := stateQueue.get.io.enq.ready // Enqueue Ready
    writeBus.ovpStreamState.get := stateQueue.get.io.deq.bits // Dequeue Bits
    stateQueue.get.io.deq.ready := dequeueFire // Dequeue Ready
  }

  // Connect the compute port to vector input of multi io queue
  for (compPortIdx <- compPorts.indices) {
    val cPort = compPorts(compPortIdx)
    // Regular Data Port
    val qPort: DecoupledIO[UInt] = queue.vecInput(compPortIdx)
    val qSync: Bool = queue.inputSyncs(compPortIdx)
    // Connect the valid
    qPort.valid := cPort.valid.get && length1D =/= 0.U
    // Connect the ready
    cPort.ready.get := qPort.ready
    // Connect the ctrl
    cPort.ctrl.get.uAct.get := cPort.ctrl.get.dAct.get
    // Connect synchronization signal that means this port is used by compute node
    // If the end of current 1D stream is reached, just enqueue it by turning off sync signal
    // (I hope it will do force enqueue)
    qSync := cPort.ctrl.get.dAct.get
    // Connect the data
    require(
      cPort.data.get.vecData.getWidth == qPort.bits.getWidth,
      s"The compute port data width ${cPort.data.get.vecData.getWidth}-bit != " +
        s"OVP input port data width ${qPort.bits.getWidth}-bit"
    )
    qPort.bits := cPort.data.get.vecData.asUInt()
  }

  // Connect the vector output of multi io queue to memory write bus
  //    queue.vecOutput.ready <<-- memReady + memReadyMask
  //    queue.vecOutput.bit/valid + option[stateQueue].valid -->> ovpValid, ovpValidMask, ovpVecTagValue
  //    depthInMemUnit - queue.numValidUnit -->  ovpAvailUnits
  //    stateQueue --> ovpStreamState is connect above in stateQueue
  // Connect Ready
  require(queue.vecOutput.length == writeBus.memReadyMask.getWidth)
  require(queue.vecOutput.length == writeBus.ovpValidMask.getWidth)
  queue.vecOutput.zipWithIndex.foreach { case (queueOutput, idx) =>
    queueOutput.ready := writeBus.memReady && writeBus.memReadyMask(idx).asBool()
  }
  // Connect Valid
  writeBus.ovpValid := dequeueFire
  // Connect Valid Mask,
  writeBus.ovpValidMask :=
    VecInit(queue.vecOutput.map(_.valid)).asUInt() & Fill(queue.vecOutput.length, dequeueFire.asUInt())
  // Connect Tag Value
  require(writeBus.ovpVecData.length == queue.vecOutput.length)
  writeBus.ovpVecData.zip(queue.vecOutput).foreach { case (memData, qOutput) =>
    // Bit width should be same
    require(memData.getWidth == qOutput.bits.getWidth)
    // Connect
    memData := qOutput.bits
  }
  // Connect number of available unit in queue
  writeBus.ovpAvailUnits := queue.finalDepth.U - queue.count

  // Connect to the output synchronization signal of inner queue, since memory write side does not require sync
  // so we connect all sync signal with false, means no sync required
  queue.outputSyncs.foreach { sync => sync := false.B }

  // If OVP is Xbar based OVP, plesae connect the configuration bitstream to output xbar routing
  if (ovpNode.vpImpl == FullXBarVP) {
    require(queue.inRoute.length == configBits.get.xbarRoute.length)
    queue.inRoute.zip(configBits.get.xbarRoute).foreach { case (route, routeCfg) =>
      require(
        route.getWidth == routeCfg.getWidth,
        s"XBar routing is ${route.getWidth}-bit but config bits has ${routeCfg.getWidth}-bit routing"
      )
      route := routeCfg
    }
  }

  /* ------------------------- Finite State Machine         ------------------------- */

  // Update the memory data type register
  memDataType match {
    case Some(value) =>
      when(writeBus.newMemDataType) {
        value := writeBus.memDataType.getOrElse(0.U)
      }
    case None =>
  }

  // Calculate the next state for remaining Length 1D
  val newL1D_sub_Borrow: UInt = writeBus.memLength1D
  val moreNewL1D:        Bool = newL1D_sub_Borrow > 0.U
  val l1D_sub_numComp:   UInt = length1D - numActCompStrUnit
  // FSM for update remain Length 1D
  when(writeBus.newMemLength1D) {
    // Update Length 1D when new 1D stream is issued
    length1D := Mux(moreNewL1D, newL1D_sub_Borrow, 0.U)
  }.elsewhen(continue1D && queue.enqFire) {
    // subtract number of active compute port when continue 1D stream and enqueue fired
    length1D := l1D_sub_numComp
  }.elsewhen(end1D && queue.enqFire) {
    // when fired at the end of current 1D stream, borrow element from next 1D stream
    length1D := 0.U
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Report the status of vector port to stream dispatcher
  // Busy = used by any memory || not empty
  ovpStatus.alive := RegNext(writeBus.usedByMem || !queue.empty)

  // Report config status
  ovpStatus.configured := confVP()

  // Report data availability in queue
  ovpStatus.hasData := !queue.empty

  // Report Firing status on compute side
  ovpStatus.compFired := queue.enqFire

  // Report firing status on memory side
  ovpStatus.memFired := queue.deqFire

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */

  /* ------------------------- Utility                      ------------------------- */

  // Aggregate the Memory Side Parameter, Create a memory write bus, connect memory port to it
  def aggMemWritePorts: OVPWriteBus = {
    // Generate Write Bus Wire
    val writeBus: OVPWriteBus = WireInit(
      0.U.asTypeOf(
        new OVPWriteBus(
          ovpParam = ovpNode,
          busWidth = memWidth,
          busUnitBits = memUnitBits,
          depthBits = memWriteParams.map(_.depthBits) max,
          stated = supportState,
          maxDataTypeBits = maxDataTypeBits,
          maxL1DBits = maxL1DBits
        )
      )
    )
    // Whether this ovp is used by any memory nodes
    writeBus.usedByMem := VecInit(memWritePorts.map(_.usedByMem)).asUInt().orR()
    // last assign has lowest priority
    require(memWritePorts.length == memWriteParams.length)
    for ((memWrite, memParam) <- memWritePorts.zip(memWriteParams)) {
      // Connect to each memory write port for default case
      memWrite.ovpValid := false.B
      memWrite.ovpValidMask := 0.U
      vecDataConnect(
        memWrite.ovpVecData,
        VecInit(Seq.fill(memWrite.ovpVecData.length)(0.U(memWrite.ovpVecData.head.getWidth.W)))
      )
      memWrite.ovpAvailUnits := 0.U
      memWrite.ovpStreamState match {
        case Some(value) => value := 0.U.asTypeOf(new StreamState)
        case None        =>
      }
      // Since there will be only one memory take the bus at a time, so simple when should work
      when(memWrite.usedByMem) {
        // Connect OVP Bus <-- MEMs
        writeBus.memReady := memWrite.memReady
        writeBus.memReadyMask := memWrite.memReadyMask
        writeBus.newMemDataType := memWrite.newMemDataType
        writeBus.memDataType match {
          case Some(value) =>
            value := {
              // Register Engine has fix memory stream type (scalar)
              if (memParam.memNode.isREG) log2Ceil(memParam.memNode.XLEN / memParam.memNode.memUnitBits).U
              else memWrite.memDataType.getOrElse(0.U)
            }
          case None =>
        }
        writeBus.newMemLength1D := memWrite.memLength1D.valid
        // Memory Length 1D for Register Engine is always 1
        writeBus.memLength1D := {
          if (memParam.memNode.isREG) 1.U else memWrite.memLength1D.bits
        }
        // Connect OVP Bus --> MEMs
        memWrite.ovpValid := writeBus.ovpValid
        memWrite.ovpValidMask := writeBus.ovpValidMask
        vecDataConnect(memWrite.ovpVecData, writeBus.ovpVecData)
        memWrite.ovpAvailUnits := writeBus.ovpAvailUnits
        memWrite.ovpStreamState match {
          case Some(value) => value := writeBus.ovpStreamState.getOrElse(0.U.asTypeOf(new StreamState))
          case None        =>
        }
      }
    }
    writeBus
  }

  val memValues: Seq[UInt] = memWritePorts.zipWithIndex.map { case (x, idx) =>
    nameVecData(x.ovpVecData, name = s"MemWrite${idx}_Value")
  }
  val compValues: Seq[UInt] = compPorts.zipWithIndex.map { case (x, idx) =>
    nameVecData(x.data.get.vecData, name = s"CompIn${idx}_Value")
  }
  if (printDebug) {
    memValues.foreach(dontTouch(_)); compValues.foreach(dontTouch(_))
  }
}

object OutputVectorPortImp extends App {
  var samples = ListBuffer[(Boolean, Boolean, Int, Int, Int, Boolean, Int)]()
  println(s"Generating Values")

  for (stated <- Seq(true, false)) {
    for (discard <- Seq(true, false)) {
      for (depth <- Seq(2, 4, 8, 16, 32)) {
        for (input <- 1 until 16) {
          for (output <- 1 until 16) {
            for (padding <- Seq(true, false)) {
              for (busWidth <- Seq(8, 16, 32, 64)) {
                if (input > 1 || !stated) {
                  if (!(!stated && padding)) {
                    samples += ((stated, discard, depth, input, output, padding, busWidth))
                  }
                }
              } // End of BusWidth Exploration
            } // End of Padding Exploration
          } // End of Output Exploration
        } // End of Input Exploration
      } // End of Depth Exploration
    } // End of Discard Exploration
  } // End Of Stated Exploration

  val samplesList = scala.util.Random.shuffle(samples.toList)

  println(s"Total number of configurations: ${samplesList.length}")
  println(s"Start to generate the configurations")

  val iter_number = min(100000, samplesList.length)

  for (n <- 0 to iter_number) {
    val (stated, discard, depth, input, output, padding, busWidth) = samplesList(n)
    println(
      s"Generating configuration $n/$iter_number: stated=$stated, discard=$discard, depth=$depth, input=$input, output=$output, padding=$padding, busWidth=$busWidth"
    )
    val cdeParam: Parameters = new Config((site, here, up) => {
      case OVPNode =>
        new OVPNodeParameters(
          vpImpl = NonXBarVP,
          nodeId = 0,
          depthByte = depth,
          vpStated = stated,
          discardOVP = discard
        )
      case XLen => 64
    })

    val ovpNode: OVPNodeParameters = cdeParam(OVPNode)
    val reconfParam: Option[CompReconfEdge] =
      if (ovpNode.vpImpl == FullXBarVP) Some(CompReconfEdge(p = cdeParam))
      else None
    // The dummy memory that connect to this IVP
    val memNode: MemNodeParameters =
      MemNodeParameters.DMA.copy(readWidth = busWidth, writeWidth = busWidth, LinearPadding = padding)
    // The dummy compute that connect from this IVP
    val compNode: CompNodeParameters = CompNodeParameters(nodeId = 0, compUnitBits = 64)
    // Generate Input / Output Parameters
    val inParams:  Seq[CompDirEdgeParameters] = Seq.fill(input)(new CompDirEdgeParameters(true, compNode, cdeParam))
    val outParams: Seq[OVP2MemParameter] = Seq.fill(output)(OVP2MemParameter(ovpNode, memNode))
    // Generate Verilog
    (new ChiselStage).emitVerilog(
      new OutputVectorPortImp(ovpNode, reconfParam, outParams, inParams)(cdeParam),
      Array(
        "--full-stacktrace",
        "--target-dir",
        "/data/dkupsh/output_vector_port/data/" + input + "-I_" + output + "-O_" + depth + "-D_" + stated + "-S_" + discard + "-E_" + busWidth + "-B_" + padding + "-P"
      )
    )

  }
}
