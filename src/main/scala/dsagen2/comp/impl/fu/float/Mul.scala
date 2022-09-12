package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.config.exception.CompException.floatException
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.comp.impl.ip.FMul_D64
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FloatGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import dsagen2.util.RegUtil.RegNextN
import hardfloat.{MulRecFN, fNFromRecFN, recFNFromFN}

import scala.collection.Set
import scala.collection.mutable.ListBuffer

object Mul extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.FloatMul)
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
    // check data Path Bits
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    val inputValid:   Bool = VecInit(inputValids).asUInt().andR()
    // Switch between ASIC backend and FPGA backend
    if (isFPGA) {
      // For now only 64-bit floating point operation are support
      require(opDataTypeSet.forall(x => x.dataType.get.compBits == 64 && x.dataType.get.unitBits == 64))
      require(A.getWidth == 64 && B.getWidth == 64)
      // Get FPGA IP for FMul
      val fmul: FMul_D64 = Module(new FMul_D64(stage = latency))
      // Connect clock
      fmul.io.aclk := clock.asBool()
      // Connect input operands and valid
      fmul.io.s_axis_a_tdata := A
      fmul.io.s_axis_a_tvalid := inputValids.head
      fmul.io.s_axis_b_tdata := B
      fmul.io.s_axis_b_tvalid := inputValids(1)
      // Connect ready
      fmul.io.m_axis_result_tready := outputReadys.head
      // Construct inputReady exception signal
      val inputReady: Bool =
        Mux(inputValid, fmul.io.s_axis_a_tready && fmul.io.s_axis_b_tready, false.B)
      val excp: EnumBundle = Wire(new EnumBundle(Set.empty))
      // Return
      (Seq(fmul.io.m_axis_result_tdata), Seq(fmul.io.m_axis_result_tvalid), Seq(inputReady, inputReady), excp)
    } else {
      // Create output LUT for each granularity
      // opcode -> (outputBits, exception)
      val outputLut: Seq[(UInt, (UInt, UInt))] = opDataTypeSet.unitBitsSet.toSeq.flatMap { unitBits =>
        // Finds all operation+dataType set under this unitBits
        val OPTs: Set[DsaOperDataType] = opDataTypeSet.filter(x => x.dataType.get.unitBits == unitBits)
        // The widest vecWidth of this unitBits
        val maxVecWidth: Int = OPTs.maxVecWidth
        val mulFNs:      Seq[MulRecFN] = Seq.fill(maxVecWidth)(Module(new MulRecFN(exp(unitBits), sig(unitBits))))
        mulFNs.zipWithIndex.foreach { case (mulFN, vecIdx) =>
          val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val fA: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iA)
          val fB: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iB)
          mulFN.io.a := fA
          mulFN.io.b := fB
          mulFN.io.roundingMode := hardfloat.consts.round_near_even
          mulFN.io.detectTininess := hardfloat.consts.tininess_beforeRounding
        }
        // Loop overall opcodes of this unitBits
        val partialOutputLut: Seq[(UInt, (UInt, UInt))] = OPTs.toSeq.map { case opt @ DsaOperDataType(_, dataType, _) =>
          val vecWidth:   Int = dataType.get.vecWidth
          val exceptions: ListBuffer[EnumBundle] = ListBuffer()
          val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
            val iRwire: UInt = WireDefault(0.U(unitBits.W))
            if (vecIdx < vecWidth) {
              val mulFN:  MulRecFN = mulFNs(vecIdx)
              val iEwire: UInt = WireDefault(0.U(floatException.size.W))
              iRwire := RegNextN(fNFromRecFN(exp(unitBits), sig(unitBits), mulFN.io.out), latency).last
              require(iEwire.getWidth == mulFN.io.exceptionFlags.getWidth)
              iEwire := mulFN.io.exceptionFlags
              exceptions += iEwire.asTypeOf(new EnumBundle(floatException))
            }
            iRwire
          }
          val exception = exceptions.result().reduce(EnumBundle.merge).asUInt()
          encoding(opt).U -> ((VecInit(vecBits).asUInt(), exception))
        }
        // Sanity Check and Return
        partialOutputLut.foreach { case (_, (_, exception)) =>
          require(
            exception.getWidth == floatException.size,
            s"There are ${floatException.size} kinds of exception, but wire is ${exception.getWidth}-bit wide"
          )
        }
        partialOutputLut
      }
      // Split output Bits and exception
      val outputBitsLut: Seq[(UInt, UInt)] = outputLut.map(x => x._1 -> x._2._1)
      val outputExcpLut: Seq[(UInt, UInt)] = outputLut.map(x => x._1 -> x._2._2)
      // Output
      val inputReady: Bool = Mux(inputValid, VecInit(outputReadys).asUInt().andR(), false.B)
      val outputBits: UInt = Wire(UInt(dataPathBits.W))
      val exception:  EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(floatException)))
      EnumBundle.connect(exception, MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(floatException)))
      val outputValid: Bool = RegNextN(inputValid, latency).last
      outputBits := MuxLookup(opcode, 0.U, outputBitsLut)
      (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
    }
  }
}
