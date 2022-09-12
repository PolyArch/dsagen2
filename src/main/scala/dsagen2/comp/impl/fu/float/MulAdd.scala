package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.{Cat, MuxLookup}
import dsagen2.comp.config.exception.CompException.floatException
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.comp.impl.ip.FMulAdd_D64
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FloatGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import hardfloat.{MulAddRecFN, fNFromRecFN, recFNFromFN}

import scala.collection.Set
import scala.collection.mutable.ListBuffer

object MulAdd extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] =
    Set(FloatPMulPAdd, FloatNMulPAdd, FloatPMulNAdd, FloatNMulNAdd)
  val supportDataType: collection.Set[DataType.DsaDataType] = FloatGroup
  val latency:         Int = 1
  val throughput:      Int = 1

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
    // Gather input dataPath bits
    val dataPathBit: Int = inputBits.head.getWidth
    val A:           UInt = inputBits.head
    val B:           UInt = inputBits(1)
    val C:           UInt = inputBits(2)
    // Create input valid signal
    val inputValid: Bool = VecInit(inputValids).asUInt().andR()
    // Look up the product sign
    val prodSign: Bool = {
      val prodSignLut: Seq[(UInt, Bool)] = opDataTypeSet.toSeq.map { case opt @ DsaOperDataType(operation, _, _) =>
        encoding(opt).U -> (operation match {
          case dsagen2.top.config.operation.Operation.FloatPMulPAdd => false.B
          case dsagen2.top.config.operation.Operation.FloatPMulNAdd => false.B
          case dsagen2.top.config.operation.Operation.FloatNMulPAdd => true.B
          case dsagen2.top.config.operation.Operation.FloatNMulNAdd => true.B
          case err =>
            require(requirement = false, s"Operation $err is not supported by $this")
            false.B
        })
      }
      MuxLookup(opcode, false.B, prodSignLut)
    }
    val sumSign: Bool = {
      val sumSignLut: Seq[(UInt, Bool)] = opDataTypeSet.toSeq.map { case opt @ DsaOperDataType(operation, _, _) =>
        encoding(opt).U -> (
          operation match {
            case dsagen2.top.config.operation.Operation.FloatPMulPAdd => false.B
            case dsagen2.top.config.operation.Operation.FloatPMulNAdd => true.B
            case dsagen2.top.config.operation.Operation.FloatNMulPAdd => false.B
            case dsagen2.top.config.operation.Operation.FloatNMulNAdd => true.B
            case err                                                  => require(requirement = false, s"Operation $err is not supported by $this"); false.B
          }
        )
      }
      MuxLookup(opcode, false.B, sumSignLut)
    }
    val maddOp: UInt = Cat(prodSign, sumSign)
    // Switch between ASIC implementation or FPGA backend
    if (isFPGA) {
      // For now only 64-bit double MulAdd supported
      require(opDataTypeSet.forall(x => x.dataType.get.compBits == 64 && x.dataType.get.unitBits == 64))
      require(A.getWidth == 64 && B.getWidth == 64 & C.getWidth == 64)
      // Get FPGA IP for FMulAdd
      val fmadd: FMulAdd_D64 = Module(new FMulAdd_D64(stage = latency))
      // Connect clock
      fmadd.io.aclk := clock.asBool()
      // Construct signed operands based on opcode
      val sA: UInt = Wire(UInt(64.W))
      val sB: UInt = Wire(UInt(64.W))
      val sC: UInt = Wire(UInt(64.W))
      sA := Cat(A(63) ^ maddOp(1), A(62, 0))
      sB := B
      sC := Cat(C(63) ^ maddOp(0), C(62, 0))
      // Connect input operands and valid
      fmadd.io.s_axis_a_tdata := sA
      fmadd.io.s_axis_a_tvalid := inputValids.head
      fmadd.io.s_axis_b_tdata := sB
      fmadd.io.s_axis_b_tvalid := inputValids(1)
      fmadd.io.s_axis_c_tdata := sC
      fmadd.io.s_axis_c_tvalid := inputValids(2)
      // Connect output ready
      fmadd.io.m_axis_result_tready := outputReadys.head
      // Construct input ready signal and exception
      val inputReady: Bool =
        Mux(inputValid, fmadd.io.s_axis_a_tready && fmadd.io.s_axis_b_tready && fmadd.io.s_axis_c_tready, false.B)
      val excp: EnumBundle = Wire(new EnumBundle(Set.empty))
      // Return
      (
        Seq(fmadd.io.m_axis_result_tdata),
        Seq(fmadd.io.m_axis_result_tvalid),
        Seq(inputReady, inputReady, inputReady),
        excp
      )
    } else {
      // Loop over all granularity
      val outputBitsExcpLut: Seq[(UInt, (UInt, UInt))] = opDataTypeSet.unitBitsSet.toSeq.flatMap { unitBits =>
        // Find all operation+dataType of this granularity
        val OPTs:        Set[DsaOperDataType] = opDataTypeSet.filter(x => x.dataType.get.unitBits == unitBits)
        val maxVecWidth: Int = OPTs.maxVecWidth
        val maddFNs:     Seq[MulAddRecFN] = Seq.fill(maxVecWidth)(Module(new MulAddRecFN(exp(unitBits), sig(unitBits))))

        // Connect input to each floating module
        maddFNs.zipWithIndex.foreach { case (maddFN, vecIdx) =>
          val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iC: UInt = C((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val fA: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iA)
          val fB: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iB)
          val fC: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iC)
          require(fA.getWidth == fB.getWidth && fB.getWidth == fC.getWidth && fA.getWidth == iA.getWidth + 1)
          maddFN.io.op := maddOp
          maddFN.io.a := fA
          maddFN.io.b := fB
          maddFN.io.c := fC
          maddFN.io.roundingMode := hardfloat.consts.round_near_even
          maddFN.io.detectTininess := hardfloat.consts.tininess_beforeRounding
        }
        // Collect for each operation of this granularity
        val partialBitsExcpLut: Seq[(UInt, (UInt, UInt))] = OPTs.toSeq.map {
          case opt @ DsaOperDataType(operation, dataType, _) =>
            val vecWidth:   Int = dataType.get.vecWidth
            val exceptions: ListBuffer[EnumBundle] = ListBuffer()
            val vecBits: Seq[UInt] = (0 until dataPathBit / unitBits).map { vecIdx =>
              val iRwire: UInt = WireDefault(0.U(unitBits.W))
              if (vecIdx < vecWidth) {
                val maddFN = maddFNs(vecIdx)
                val iEwire: EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(floatException)))
                iRwire := fNFromRecFN(exp(unitBits), sig(unitBits), maddFN.io.out)
                EnumBundle.connect(iEwire, maddFN.io.exceptionFlags.asTypeOf(new EnumBundle(floatException)))
                exceptions += iEwire
              }
              iRwire
            }
            val exceptionUInt: UInt =
              exceptions.result().reduce(EnumBundle.merge).asUInt()
            encoding(opt).U -> ((VecInit(vecBits).asUInt(), exceptionUInt))
        }
        partialBitsExcpLut
      }
      // Split output bits and exception
      val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._1)
      val outputExcpLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._2)
      // Output
      val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
      val inputReady:     Bool = Mux(inputValid, VecInit(outputReadys).asUInt().andR(), false.B)
      val outputBitsWire: UInt = Wire(UInt(dataPathBit.W))
      outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
      val exception: EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(floatException)))
      EnumBundle.connect(exception, MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(floatException)))
      val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
      (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady, inputReady), exception)
    }
  }
}
