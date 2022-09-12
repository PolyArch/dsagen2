package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util._
import dsagen2.comp.config.exception.CompException.floatException
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.comp.impl.ip.{FDiv_D64, FSqrt_D64}
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.enumeration.EnumEncodeMethod
import dsagen2.top.config.operation.DataType.FloatGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import hardfloat.{DivSqrtRecFN_srt4, fNFromRecFN, recFNFromFN}

import scala.collection.Set
import scala.collection.mutable.ListBuffer

object DivSqrt extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.FloatDiv, Operation.FloatSqrt)
  val supportDataType:  collection.Set[DataType.DsaDataType] = FloatGroup
  val latency:          Int = 5
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
    // Gather inputs and bitWidth
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    // Create input valid signal
    val inputValid: Bool = {
      val validLut = opDataTypeSet.toSeq.map { case opt @ DsaOperDataType(operation, dataType, predefinedImpl) =>
        operation match {
          case dsagen2.top.config.operation.Operation.FloatDiv =>
            encoding(opt).U -> (inputValids.head && inputValids(1))
          case dsagen2.top.config.operation.Operation.FloatSqrt => encoding(opt).U -> inputValids.head
          case err                                              => require(requirement = false, s"Operation $err is not supported by $this"); -1.U -> false.B
        }
      }
      MuxLookup(opcode, false.B, validLut)
    }
    // Mux Lookup whether it is sqrt operation
    val isSqrt: Bool = {
      val sqrtLut: Seq[(UInt, Bool)] = opDataTypeSet.toSeq.map { case opt @ DsaOperDataType(operation, _, _) =>
        operation match {
          case dsagen2.top.config.operation.Operation.FloatDiv  => encoding(opt).U -> false.B
          case dsagen2.top.config.operation.Operation.FloatSqrt => encoding(opt).U -> true.B
          case err                                              => require(requirement = false, s"Operation $err is not supported by $this"); -1.U -> false.B
        }
      }
      MuxLookup(opcode, false.B, sqrtLut)
    }
    // Switch between ASIC implementation or FPGA backend
    if (isFPGA) {
      // For now only 64-bit double Div and Sqrt supported
      require(opDataTypeSet.forall(x => x.dataType.get.compBits == 64 && x.dataType.get.unitBits == 64))
      require(A.getWidth == 64 && B.getWidth == 64)
      // Get FPGA IP for FDiv and FSqrt
      val fdiv:  FDiv_D64 = Module(new FDiv_D64(stage = latency))
      val fsqrt: FSqrt_D64 = Module(new FSqrt_D64(stage = latency))
      // Connect clocks
      fdiv.io.aclk := clock.asBool()
      fsqrt.io.aclk := clock.asBool()
      // Connect input operands and valid
      fdiv.io.s_axis_a_tdata := A
      fdiv.io.s_axis_a_tvalid := inputValids.head && !isSqrt
      fdiv.io.s_axis_b_tdata := B
      fdiv.io.s_axis_b_tvalid := inputValids(1) && !isSqrt
      fsqrt.io.s_axis_a_tdata := A
      fsqrt.io.s_axis_a_tvalid := inputValids.head && isSqrt
      // Connect output ready
      fdiv.io.m_axis_result_tready := outputReadys.head && !isSqrt
      fsqrt.io.m_axis_result_tready := outputReadys.head && isSqrt
      // Connect output valid and bits
      val outputBits:  UInt = Mux(isSqrt, fsqrt.io.m_axis_result_tdata, fdiv.io.m_axis_result_tdata)
      val outputValid: Bool = Mux(isSqrt, fsqrt.io.m_axis_result_tvalid, fdiv.io.m_axis_result_tvalid)
      // Construct input ready signal and exception
      val inputReady: Bool = Mux(
        isSqrt,
        fsqrt.io.s_axis_a_tready && fsqrt.io.s_axis_a_tvalid,
        fdiv.io.s_axis_a_tready && fdiv.io.s_axis_b_tready && fdiv.io.s_axis_a_tvalid && fdiv.io.s_axis_b_tvalid
      )
      val excp: EnumBundle = Wire(new EnumBundle(Set.empty))
      // Return
      (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), excp)
    } else {
      // Loop over all granularity
      // opcode -> (outputBits, exception, valid, ready)
      val outputLut: Seq[(UInt, (UInt, UInt, Bool, Bool))] = opDataTypeSet.unitBitsSet.toSeq.flatMap { unitBits =>
        // Find all operation+dataType of this unitBits
        val OPTs: Set[DsaOperDataType] = opDataTypeSet.filter { opt => opt.dataType.get.unitBits == unitBits }
        // The widest vectorization of this unitBits
        val maxVecWidth: Int = OPTs.maxVecWidth
        val divFNs: Seq[DivSqrtRecFN_srt4] =
          Seq.fill(maxVecWidth)(Module(new DivSqrtRecFN_srt4(exp(unitBits), sig(unitBits))))
        // Connect input to Floating Module
        divFNs.zipWithIndex.foreach { case (divFN, vecIdx) =>
          val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val fA: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iA)
          val fB: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iB)
          divFN.io.inValid := inputValid
          divFN.io.sqrtOp := isSqrt
          divFN.io.a := fA
          divFN.io.b := fB
          divFN.io.roundingMode := hardfloat.consts.round_near_even //TODO, do we need to encode it in opcode?
          divFN.io.detectTininess := hardfloat.consts.tininess_beforeRounding //TODO, do we need to encode it in opcode?
        }
        // Loop over all opcodes of this unitBits, lookup for (bits, exception, valid and ready) under this unitBits
        val partialBitsExcpLut: Seq[(UInt, (UInt, UInt, Bool, Bool))] = OPTs.toSeq.map {
          case opt @ DsaOperDataType(operation, dataType, _) =>
            val vecWidth:   Int = dataType.get.vecWidth
            val exceptions: ListBuffer[EnumBundle] = ListBuffer()
            val valids:     ListBuffer[Bool] = ListBuffer()
            val readys:     ListBuffer[Bool] = ListBuffer()
            val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
              val iRwire: UInt = WireDefault(0.U(unitBits.W))
              if (vecIdx < vecWidth) {
                val iEwire: EnumBundle = WireDefault(
                  0.U.asTypeOf(
                    new EnumBundle(
                      floatException, // specify the exception set of floating operation
                      EnumEncodeMethod.maskEncode, // I am not sure whether it is one hot, but it is mask encoded
                      supportInvalid = false, // source of Enum, no invalid from upstream
                      isOneHot = false // not sure whether it is one hot
                    )
                  )
                )
                val iValid: Bool = WireDefault(false.B)
                val iReady: Bool = WireDefault(false.B)
                val divFN:  DivSqrtRecFN_srt4 = divFNs(vecIdx)
                iRwire := fNFromRecFN(exp(unitBits), sig(unitBits), divFN.io.out)
                EnumBundle.connect(iEwire, divFN.io.exceptionFlags.asTypeOf(new EnumBundle(floatException)))
                operation match {
                  case dsagen2.top.config.operation.Operation.FloatDiv  => iValid := divFN.io.outValid_div
                  case dsagen2.top.config.operation.Operation.FloatSqrt => iValid := divFN.io.outValid_sqrt
                  case err                                              => require(requirement = false, s"Operation $err cannot be supported by $this")
                }
                iReady := divFN.io.inReady
                readys += iReady
                valids += iValid
                exceptions += iEwire
              }
              iRwire
            }
            val exception: UInt = exceptions.result().reduce(EnumBundle.merge).asUInt()
            encoding(opt).U -> (
              (
                VecInit(vecBits).asUInt(),
                exception,
                VecInit(valids).asUInt().andR(),
                VecInit(readys).asUInt().andR()
              )
            )
        }
        // Sanity Check and Return
        partialBitsExcpLut.foreach { case (_, (_, excep, _, _)) =>
          require(
            excep.getWidth == floatException.size,
            s"There are ${floatException.size} kinds of exceptions, but wire is ${excep.getWidth}-bit"
          )
        }
        partialBitsExcpLut
      }
      // Split output bits, exception, valid, ready
      val outputBitsLut:  Seq[(UInt, UInt)] = outputLut.map(x => x._1 -> x._2._1)
      val outputExcpLut:  Seq[(UInt, UInt)] = outputLut.map(x => x._1 -> x._2._2)
      val outputValidLut: Seq[(UInt, Bool)] = outputLut.map(x => x._1 -> x._2._3)
      val inputReadyLut:  Seq[(UInt, Bool)] = outputLut.map(x => x._1 -> x._2._4)
      // Output
      val outputValid:    Bool = MuxLookup(opcode, false.B, outputValidLut)
      val inputReady:     Bool = Mux(inputValid, MuxLookup(opcode, false.B, inputReadyLut), false.B)
      val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
      outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
      val exception: EnumBundle = Wire(new EnumBundle(floatException))
      EnumBundle.connect(exception, MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(floatException)))
      val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
      (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
    }
  }
}
