package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

object Select extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.Select)
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
    // gather operand and data path
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    val C:            UInt = inputBits(2)
    // Generate result for each opcode
    val outputBitsLut: Seq[(UInt, UInt)] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(operation, dataType, _) =>
        val unitBits: Int = dataType.get.unitBits
        val vecWidth: Int = dataType.get.vecWidth
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iRwire: UInt = Wire(UInt(unitBits.W))
          if (vecIdx < vecWidth) {
            val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iC: Bool = C(vecIdx * unitBits).asBool() // use LSB bit as select
            iRwire := Mux(iC, iA, iB)
          } else {
            iRwire := 0.U
          }
          iRwire
        }
        encoding(opt).U -> VecInit(vecBits).asUInt()
      case err =>
        require(requirement = false, s"Operation $err cannot be supported by $this")
        -1.U -> -1.U
    }
    // Output Generate
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    val exception:  EnumBundle = Wire(new EnumBundle(Set.empty))
    // Return
    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady, inputReady), exception)
  }
}
