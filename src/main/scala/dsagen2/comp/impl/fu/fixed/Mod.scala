package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.config.exception.CompException.FixedDividedByZero
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation.FixedMod
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

import scala.collection.mutable.ListBuffer

object Mod extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(FixedMod)
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

    // Gather the input Bits and data path width
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)

    // Generate Output Lookup Table
    val outputBitsExcpLut: Seq[(UInt, (UInt, Bool))] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(_, dataType, _) =>
        val unitBits:    Int = dataType.get.unitBits
        val vecWidth:    Int = dataType.get.vecWidth
        val divideZeros: ListBuffer[Bool] = ListBuffer[Bool]()
        val vecResults: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iRwire: UInt = Wire(UInt(unitBits.W))
          if (vecIdx < vecWidth) {
            val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            iRwire := (if (dataType.get.isSigned) (iA.asSInt() % iB.asSInt()).asUInt() else iA % iB)
            val divideZero: Bool = iB === 0.U
            divideZeros += divideZero
          } else {
            iRwire := 0.U
          }
          iRwire
        }
        encoding(opt).U -> ((VecInit(vecResults).asUInt(), VecInit(divideZeros.result()).asUInt().orR()))
      case err =>
        require(requirement = false, s"Operation DataType: $err is not supported by $this")
        -1.U -> ((0.U, false.B))
    }
    // Split out bits and exception (divided by zero)
    val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._1)
    val outputExcpLut: Seq[(UInt, Bool)] = outputBitsExcpLut.map(x => x._1 -> x._2._2)
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
    val exception:      EnumBundle = Wire(new EnumBundle(Set(FixedDividedByZero)))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    exception.enumHardwareField.get := MuxLookup(opcode, false.B, outputExcpLut).asUInt()
    val outputBits = Mux(outputValid, outputBitsWire, 0.U)
    // Return
    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
  }
}
