package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

/** High-Low Add
  *
  * For example: A[63:0]
  * B = HLAdd(A) --> B[63:0] = A[63:32] + A[31:0]
  */
object HLAdd extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.FixedHLAdd)
  val supportDataType:  collection.Set[DataType.DsaDataType] = FixedGroup
  val latency:          Int = 0
  val throughput:       Int = 1

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
    // Sanity check
    require(inputBits.length == 1 && inputValids.length == 1, "HLAdd is one input operation")
    require(outputReadys.length == 1, "HLAdd is one output operation")
    // Extract parameter
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    // Loop over all granularity
    val resultBitsLut: Seq[(UInt, UInt)] = opDataTypeSet.toSeq.map { case opt @ DsaOperDataType(_, dataType, _) =>
      val enc:      Int = encoding(opt)
      val unitBits: Int = dataType.get.unitBits
      val vecWidth: Int = dataType.get.vecWidth
      val vecResult: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
        val iRwire: UInt = Wire(UInt(unitBits.W))
        if (vecIdx < vecWidth) {
          val iA_high: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits + unitBits / 2)
          val iA_low:  UInt = A(vecIdx * unitBits + unitBits / 2 - 1, vecIdx * unitBits)
          require(iA_low.getWidth == iA_high.getWidth && iA_high.getWidth == unitBits / 2)
          iRwire := iA_high +& iA_low
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
    outputBitsWire := MuxLookup(opcode, 0.U, resultBitsLut)
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    val inputReady: Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val exception = Wire(new EnumBundle(Set.empty))
    // Return
    (Seq(outputBits), Seq(outputValid), Seq(inputReady), exception)
  }
}
