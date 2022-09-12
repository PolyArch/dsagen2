package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation.FixedMin
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

import scala.collection.Set

object Min extends IsFunctionUnitImplementation {
  override val supportOperation: collection.Set[Operation.DsaOperation] = Set(FixedMin)
  override val supportDataType:  collection.Set[DataType.DsaDataType] = FixedGroup
  override val latency:          Int = 0
  override val throughput:       Int = 1

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
    // Gather the input Bits and data path width
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    // Generate Output Lookup Table
    val outputBitsExcpLut: Seq[(UInt, (UInt, Bool))] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(_, dataType, _) =>
        val unitBits: Int = dataType.get.unitBits
        val vecWidth: Int = dataType.get.vecWidth
        val vecResults: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iRwire: UInt = Wire(UInt(unitBits.W))
          if (vecIdx < vecWidth) {
            val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            iRwire := (if (dataType.get.isSigned) (iA.asSInt().min(iB.asSInt())).asUInt() else iA.min(iB))
          } else {
            iRwire := 0.U
          }
          iRwire
        }
        encoding(opt).U -> ((VecInit(vecResults).asUInt(), false.B))
      case err =>
        require(requirement = false, s"Operation DataType: $err is not supported by $this")
        -1.U -> ((0.U, false.B))
    }
    // Split out bits and exception (divided by zero)
    val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._1)
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
    // No exception for Min
    val exception: EnumBundle = Wire(new EnumBundle(Set.empty))
    // Assign to the result bits
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val outputBits = Mux(outputValid, outputBitsWire, 0.U)
    // Return
    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
  }
}
