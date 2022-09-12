package dsagen2.comp.impl.fu.misc

import chipsalliance.rocketchip.config
import chisel3._
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

object BitwiseXor extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.BitwiseXor)
  val supportDataType:  collection.Set[DataType.DsaDataType] = Set.empty
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
    // Gather IO
    val dataPathBit: Int = inputBits.head.getWidth
    val A:           UInt = inputBits.head
    val B:           UInt = inputBits(1)
    // Sanity Check
    require(opDataTypeSet.size == 1, s"$this should only have one operation, which is $this, but $opDataTypeSet")
    require(
      A.getWidth == B.getWidth && B.getWidth == dataPathBit,
      s"Bitwise operation should be applied to operands whose bitwidth is same"
    )
    // Calculate the result
    val C: UInt = A ^ B
    // Output
    val outputValid: Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:  Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBits:  UInt = Mux(outputValid, C, 0.U)
    val exception:   EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(Set.empty)))
    // Return
    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
  }
}
