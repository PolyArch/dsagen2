package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.config.exception.CompException.floatException
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.comp.impl.ip.FAddSub_D64
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.enumeration.EnumEncodeMethod
import dsagen2.top.config.operation.DataType.FloatGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import dsagen2.util.RegUtil.RegNextN
import hardfloat.{AddRecFN, fNFromRecFN, recFNFromFN}

import scala.collection.Set
import scala.collection.mutable.ListBuffer

object AddSub extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.FloatAdd, Operation.FloatSub)
  val supportDataType:  collection.Set[DataType.DsaDataType] = FloatGroup
  val latency:          Int = 3
  val throughput:       Int = 1

  /** Hardware Function Template for Data Unit, will be overwrite by actual implementation
    *
    * @param inputBits     Input Bits from each operand
    * @param inputValids   Input valid bit from each operand
    * @param outputReadys  Downward Readies from each results
    * @param opcode        Opcode Wire
    * @param opDataTypeSet Operation+DataType Set for this function unit
    * @param encoding      mapping from Operation+DataType to opcode number
    * @param p             Hidden Processing Element parameters
    * @return (outputBits, outputValids, inputReadys, exception)
    */
  def implement(
    inputBits:     Seq[UInt],
    inputValids:   Seq[Bool],
    outputReadys:  Seq[Bool],
    opcode:        UInt,
    opDataTypeSet: collection.Set[OperDataType.DsaOperDataType],
    encoding:      Map[OperDataType.DsaOperDataType, Int],
    clock:         Clock,
    reset:         Reset
  )(
    implicit p: config.Parameters
  ): (Seq[UInt], Seq[Bool], Seq[Bool], EnumBundle) = {
    // Gather operand and data path bits
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    // Create input valid signal
    val inputValid: Bool = VecInit(inputValids).asUInt().andR()
    // Check whether it is performing subtract operation
    val isSub: Bool = {
      val subLut: Seq[(UInt, Bool)] = opDataTypeSet.toSeq.map { case opt @ DsaOperDataType(operation, _, _) =>
        operation match {
          case FloatAdd => encoding(opt).U -> false.B
          case FloatSub => encoding(opt).U -> true.B
          case err      => require(requirement = false, s"Operation $err is not supported by $this"); -1.U -> false.B
        }
      }
      MuxLookup(opcode, false.B, subLut)
    }
    // Switch between ASIC implementation or FPGA backend
    if (isFPGA) {
      // For now only 64-bit double Add and Subtract supported
      require(opDataTypeSet.forall(x => x.dataType.get.compBits == 64 && x.dataType.get.unitBits == 64))
      require(A.getWidth == 64 && B.getWidth == 64)
      // Get FPGA IP for FAddSub
      val faddsub: FAddSub_D64 = Module(new FAddSub_D64(stage = latency))
      // Connect clock
      faddsub.io.aclk := clock.asBool()
      // Connect input operands and valid
      faddsub.io.s_axis_a_tdata := A
      faddsub.io.s_axis_a_tvalid := inputValids.head
      faddsub.io.s_axis_b_tdata := B
      faddsub.io.s_axis_b_tvalid := inputValids(1)
      // Connect operation
      faddsub.io.s_axis_operation_tdata := isSub.asUInt()
      faddsub.io.s_axis_operation_tvalid := VecInit(inputValids).asUInt().andR()
      // Connect output ready
      faddsub.io.m_axis_result_tready := outputReadys.head
      // Construct input ready signal and exception
      val inputReady: Bool =
        Mux(
          inputValid,
          faddsub.io.s_axis_a_tready && faddsub.io.s_axis_b_tready && faddsub.io.s_axis_operation_tready,
          false.B
        )
      val excp: EnumBundle = Wire(new EnumBundle(Set.empty))
      // Return
      (Seq(faddsub.io.m_axis_result_tdata), Seq(faddsub.io.m_axis_result_tvalid), Seq(inputReady, inputReady), excp)
    } else {
      // Loop over all granularity for produce result
      val outputBitsExcpLut: Seq[(UInt, (UInt, UInt))] = opDataTypeSet.unitBitsSet.toSeq.flatMap { unitBits =>
        // Find all OperationDataType belongs to this unitBits
        val OPTs: Set[DsaOperDataType] = opDataTypeSet.filter { opt => opt.unitBits.get == unitBits }
        // calculate the max vecWidth
        val maxVecWidth: Int = OPTs.maxVecWidth
        val addFNs:      Seq[AddRecFN] = Seq.fill(maxVecWidth)(Module(new AddRecFN(exp(unitBits), sig(unitBits))))
        // Connect to the inward side
        addFNs.zipWithIndex.foreach { case (addFN, vecIdx) =>
          val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val fA: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iA)
          val fB: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iB)
          addFN.io.a := fA
          addFN.io.b := fB
          addFN.io.subOp := isSub
          addFN.io.roundingMode := hardfloat.consts.round_near_even //TODO, do we need to encode it in opcode?
          addFN.io.detectTininess := hardfloat.consts.tininess_beforeRounding //TODO, do we need to encode it in opcode?
        }
        // Loop over operation + dataType under this unitBits
        val partialBitsExcpLut: Seq[(UInt, (UInt, UInt))] = OPTs.toSeq.map {
          case opt @ DsaOperDataType(operation, dataType, _) =>
            val vecWidth:   Int = dataType.get.vecWidth
            val exceptions: ListBuffer[EnumBundle] = ListBuffer[EnumBundle]()
            val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
              // Wire for result
              val iRwire: UInt = WireDefault(0.U(unitBits.W))
              if (vecIdx < vecWidth) {
                // Wire for exception
                val iExcep: EnumBundle = Wire(new EnumBundle(floatException, EnumEncodeMethod.maskEncode))
                val addFN:  AddRecFN = addFNs(vecIdx)
                iRwire := RegNextN(fNFromRecFN(exp(unitBits), sig(unitBits), addFN.io.out), latency).last
                EnumBundle.connect(
                  iExcep,
                  addFN.io.exceptionFlags.asTypeOf(new EnumBundle(floatException, EnumEncodeMethod.maskEncode))
                )
                exceptions += iExcep
              }
              iRwire
            }
            encoding(opt).U -> (
              (
                VecInit(vecBits).asUInt() /*Merge result bits*/,
                exceptions.result().reduce(EnumBundle.merge).asUInt()
              )
            )
        }
        // Sanity check
        partialBitsExcpLut.foreach { case (_, (_, exception)) =>
          require(
            exception.getWidth == floatException.size,
            s"There are ${floatException.size} kinds of exceptions, but wire is ${exception.getWidth}-bit"
          )
        }
        partialBitsExcpLut
      }
      // Split output bits and exceptions
      val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(be => be._1 -> be._2._1)
      val outputExcpLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(be => be._1 -> be._2._2)
      // Output
      val inputReady: Bool = Mux(inputValid, VecInit(outputReadys).asUInt().andR(), false.B)
      val outputBits: UInt = Wire(UInt(dataPathBits.W))
      val exception:  EnumBundle = Wire(new EnumBundle(floatException, EnumEncodeMethod.maskEncode))
      EnumBundle.connect(
        exception,
        MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(floatException, EnumEncodeMethod.maskEncode))
      )
      val outputValid: Bool = RegNextN(inputValid, latency).last
      outputBits := MuxLookup(opcode, 0.U, outputBitsLut)
      // Return
      (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
    }
  }
}
