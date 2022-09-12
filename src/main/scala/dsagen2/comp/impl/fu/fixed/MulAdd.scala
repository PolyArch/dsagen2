package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.config.exception.CompException.FixedOverflow
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation.{FixedNMulNAdd, FixedNMulPAdd, FixedPMulNAdd, FixedPMulPAdd}
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

import scala.collection.mutable.ListBuffer

object MulAdd extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] =
    Set(FixedPMulPAdd, FixedPMulNAdd, FixedNMulPAdd, FixedNMulNAdd)
  val supportDataType: collection.Set[DataType.DsaDataType] = FixedGroup
  val latency:         Int = 0
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
    // Gather operand
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    val C:            UInt = inputBits(2)
    // Generate output lookup table
    val outputBitsOverLut: Seq[(UInt, (UInt, Bool))] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(operation, dataType, _) =>
        val unitBits:  Int = dataType.get.unitBits
        val vecWidth:  Int = dataType.get.vecWidth
        val overflows: ListBuffer[Bool] = ListBuffer[Bool]()
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iExtRes: UInt = Wire(UInt((2 * unitBits).W))
          val iRwire:  UInt = iExtRes(unitBits - 1, 0)
          // high part xorR is one mean high part is meaningful, which is overflow
          val overflow: Bool = iExtRes(2 * unitBits - 1, unitBits).xorR()
          // falls in vector range
          if (vecIdx < vecWidth) {
            val iA:   UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iB:   UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iC:   UInt = C((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val prod: UInt = Wire(UInt((2 * unitBits).W))
            if (dataType.get.isSigned) prod := (iA.asSInt() * iB.asSInt()).asUInt()
            else prod := iA * iB
            operation match {
              case FixedPMulPAdd => iExtRes := (prod + iC)
              case FixedPMulNAdd => iExtRes := (prod - iC)
              case FixedNMulPAdd => iExtRes := (-prod + iC)
              case FixedNMulNAdd => iExtRes := (-prod - iC)
              case err           => require(requirement = false, s"Operation DataType $err cannot be supported by $this")
            }
          } else {
            iExtRes := 0.U
          }
          overflows += overflow
          iRwire
        } // Concat vector result    any element is overflow mean overflow
        encoding(opt).U -> (VecInit(vecBits).asUInt(), VecInit(overflows).asUInt().orR())
      case err =>
        require(requirement = false, s"Operation DataType: $err is not supported by $this")
        -1.U -> (-1.U, false.B)
    }
    // Split Result LUT and overflow LUT
    val outputBitsLUT: Seq[(UInt, UInt)] = outputBitsOverLut.map { case (int, (bits, _)) => int -> bits }
    val outputOverLUT: Seq[(UInt, Bool)] = outputBitsOverLut.map { case (int, (_, bool)) => int -> bool }
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
    val overflow:       Bool = MuxLookup(opcode, false.B, outputOverLUT)
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLUT)
    val outputBits = Mux(outputValid, outputBitsWire, 0.U)
    val exception: EnumBundle = Wire(new EnumBundle(Set(FixedOverflow)))
    exception.enumHardwareField match {
      case Some(of) => of := overflow.asUInt()
      case None     => require(requirement = false, "Why MulAdd does not have overflow exception?")
    }
    // Return
    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady, inputReady), exception)
  }
}
