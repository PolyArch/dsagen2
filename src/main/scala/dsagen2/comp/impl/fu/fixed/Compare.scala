package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

import scala.collection.Set

/** This is an not composable fixed point number comparator, A ?? (<, <=, =, !=, >=, >) B
  */
object Compare extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.Compare)
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
    // extract data path bits
    val dataPathBits: Int = inputBits.head.getWidth
    val A = inputBits.head
    val B = inputBits(1)
    // Build Look Up Table, TODO: composable design in the future
    val optSeq: Seq[DsaOperDataType] = opDataTypeSet.toSeq
    val outputBitsLUT: Seq[(UInt, UInt)] =
      optSeq.map(encoding(_)).zip(optSeq).map { case (enc, DsaOperDataType(operation, dataType, _)) =>
        val unitBits: Int = dataType.get.unitBits
        val vecWidth: Int = dataType.get.vecWidth
        val vecResult: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          if (vecIdx < vecWidth) {
            val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
            val iC: UInt = operation match {
              case Operation.Compare =>
                if (dataType.get.isSigned) {
                  val gt: Bool = iA.asSInt() > iB.asSInt()
                  val eq: Bool = iA.asSInt() === iB.asSInt()
                  val lt: Bool = iA.asSInt() < iB.asSInt()
                  Cat(gt, eq, lt)
                } else {
                  val gt: Bool = iA > iB
                  val eq: Bool = iA === iB
                  val lt: Bool = iA < iB
                  Cat(gt, eq, lt)
                }
              case err => require(requirement = false, s"Operation $err is not supported by $this"); 0.U
            }
            val iCwire: UInt = Wire(UInt(unitBits.W))
            iCwire := iC
            iCwire
          } else {
            0.U(unitBits.W)
          }
        }
        enc.U -> VecInit(vecResult).asUInt()
      }
    // Output Valid
    val outputValid = VecInit(inputValids).asUInt().andR()
    val outputBitsWire: UInt = MuxLookup(opcode, 0.U, outputBitsLUT)
    val outputBits = Mux(outputValid, outputBitsWire, 0.U)
    val inputReady: Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val exception:  EnumBundle = Wire(new EnumBundle(Set.empty))
    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
  }
}
