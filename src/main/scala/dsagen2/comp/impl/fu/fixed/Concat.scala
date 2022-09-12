package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.{Cat, MuxLookup}
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

/** Concat
  * Example:
  *
  * 8-bit x 1 concat
  * A = [00001010], B = [10001111]
  * Concat(A, B) = [10101111]
  *
  * 8-bit x 2 Concat
  * A = [11100001 10100101], B = [11110101 00011110]
  * Concat(A, B) = [10100101 00011110]
  */
object Concat extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.Concat)
  val supportDataType:  collection.Set[DataType.DsaDataType] = FixedGroup
  val latency:          Int = 0
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
    // Extract parameter
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    // Loop over all granularity
    val resultBitsLUT: Seq[(UInt, UInt)] = opDataTypeSet.toSeq.map { case opt @ DsaOperDataType(_, dataType, _) =>
      val enc:      Int = encoding(opt)
      val unitBits: Int = dataType.get.unitBits
      val vecWidth: Int = dataType.get.vecWidth
      val vecResult: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
        val iRwire: UInt = Wire(UInt(unitBits.W))
        if (vecIdx < vecWidth) {
          val iA:    UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iB:    UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iAlow: UInt = iA(unitBits / 2 - 1, 0)
          val iBlow: UInt = iB(unitBits / 2 - 1, 0)
          iRwire := Cat(iAlow, iBlow)
        } else {
          iRwire := 0.U
        }
        iRwire
      }
      enc.U -> VecInit(vecResult).asUInt()
    }
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, resultBitsLUT)
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    val inputReady: Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val exception = Wire(new EnumBundle(Set.empty))
    // Return
    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
  }
}
