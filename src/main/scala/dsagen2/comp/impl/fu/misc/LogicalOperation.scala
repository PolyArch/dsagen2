package dsagen2.comp.impl.fu.misc

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.AllGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.Operation.{LogicalAnd, LogicalNot, LogicalOr, LogicalXor}
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

object LogicalOperation extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(LogicalNot, LogicalAnd, LogicalOr, LogicalXor)
  val supportDataType:  collection.Set[DataType.DsaDataType] = AllGroup
  val latency:          Int = 0
  val throughput:       Int = 1

  /** Hardware Function Template for Data Unit, will be overwrite by actual implementation
    * Hardware module to support logical operation, this operation is cheap, so I don't want to do composable things
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
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    // Generate outputLut
    val outputBitsLut: Seq[(UInt, UInt)] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(operation, Some(dataType), _) =>
        val unitBits: Int = dataType.unitBits
        val vecWidth: Int = dataType.vecWidth
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iRwire: UInt = WireDefault(0.U(unitBits.W))
          if (vecIdx < vecWidth) {
            val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            operation match {
              case LogicalNot => iRwire := !iA
              case LogicalAnd =>
                val B:  UInt = inputBits(1)
                val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
                iRwire := iA.orR() && iB.orR()
              case LogicalOr =>
                val B:  UInt = inputBits(1)
                val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
                iRwire := iA.orR() || iB.orR()
              case LogicalXor =>
                val B:  UInt = inputBits(1)
                val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
                iRwire := (iA.orR() && !iB.orR()) || (!iA.orR() && iB.orR())
              case err => require(requirement = false, s"Operation $err is not supported by $this")
            }
          }
          iRwire
        }
        encoding(opt).U -> VecInit(vecBits).asUInt()
      case err =>
        require(requirement = false, s"Operation $err is not supported by $this")
        -1.U -> -1.U
    }
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = WireDefault(0.U(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    val inputReadys: Seq[Bool] =
      if (opDataTypeSet.operations.forall(_ == LogicalNot))
        Seq(inputReady) // if only Logical Not exists, then one ready
      else Seq(inputReady, inputReady) // two input readys
    (Seq(outputBits), Seq(outputValid), inputReadys, WireDefault(0.U.asTypeOf(new EnumBundle(Set.empty))))
  }
}
