package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.config.exception.CompException.ComputationException
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.enumeration.EnumEncodeMethod
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._

import scala.collection.Set

/** Single Module for Operation + DataType, generate Function Unit Module out of it. All operations has the same
  * hardware implementation
  *
  * @param opDataTypeSet     A Set of operation and data type with the same hardware implementation
  * @param dataPathBits      the data path bits of PE, p(CompNode).compBits
  * @param opcodeMapping     the opcode mapping for operations
  * @param numTotalOperation the total amount of operation of ALU, basically used for #opcodeBits
  */
class FunctionUnitModule(
  opDataTypeSet:     Set[DsaOperDataType],
  opcodeMapping:     Map[DsaOperDataType, Int],
  dataPathBits:      Int,
  numTotalOperation: Int
)(
  implicit val p: Parameters)
    extends MultiIOModule {

  override def desiredName: String = s"FunctionUnit_${opDataTypeSet.compactName}"

  // TODO: this step is also critical (as WithAluModule.operationGrouping),
  //    which pick one implementation out of common implementation set
  val impl: IsFunctionUnitImplementation = {
    // Find the implementation that can support all of the of the opcode provided
    val commonImpl: Set[IsFunctionUnitImplementation] = opDataTypeSet.map(_.getImpls).reduce(_ intersect _)
    require(commonImpl.nonEmpty, s"Operation+DataType: $opDataTypeSet set does not have common implementation")
    //TODO, current way is trivial, just pick the first one, we should write a algorithm to pick the best
    // implementation
    commonImpl.head
  }
  require(opcodeMapping.values.toSeq.distinct.length == opcodeMapping.size, s"The opcode encoding is not unique")

  /*Extract Parameters*/
  val maxOpcode:     Int = opDataTypeSet.maxOpcode(opcodeMapping)
  val maxNumOperand: Int = opDataTypeSet.maxNumOperand
  val maxNumResult:  Int = opDataTypeSet.maxNumResult
  val opcodeBits:    Int = log2Ceil(numTotalOperation)
  val exceptionSet:  Set[ComputationException] = opDataTypeSet.map(_.operation.getExceptions).reduce(_ ++ _)
  val maxDataBits:   Int = opDataTypeSet.maxDataBits

  /*Requirements*/
  require(dataPathBits >= 1, s"Your PE's bit width is $dataPathBits")
  require(
    maxDataBits <= dataPathBits,
    s"Operation is up to $maxDataBits-bit wide, " +
      s"but PE is only $dataPathBits-bit wide"
  )
  require(maxOpcode < numTotalOperation, s"Opcode $maxOpcode >= number of operation $numTotalOperation")

  /* Input/Output */
  val Operands: Vec[DecoupledIO[UInt]] = IO(Vec(maxNumOperand, Flipped(DecoupledIO(UInt(dataPathBits.W)))))
  val Results:  Vec[DecoupledIO[UInt]] = IO(Vec(maxNumResult, DecoupledIO(UInt(dataPathBits.W))))
  val Opcode:   Option[UInt] = if (opcodeBits > 0) Some(IO(Input(UInt(opcodeBits.W)))) else None
  val Exception: EnumBundle = IO(
    Output(
      new EnumBundle(
        enumerations = exceptionSet.asInstanceOf[Set[Enumeration#Value]],
        encMethod = EnumEncodeMethod.maskEncode,
        supportInvalid = false,
        isOneHot = false
      )
    )
  )

  /* Hardware Function */
  // Apply this function to input.bits/valid and output.ready will return
  // with output.bits/valid, input.ready
  val hardwareImplementation: IsFunctionUnitImplementation = impl

  /* Form input for hardware function */
  val inputBits:    Seq[UInt] = Operands.map(_.bits)
  val inputValids:  Seq[Bool] = Operands.map(_.valid)
  val outputReadys: Seq[Bool] = Results.map(_.ready)

  /* Adding support for decomposable operation */
  val (outputBits, outputValids, inputReadys, outputException): (Seq[UInt], Seq[Bool], Seq[Bool], EnumBundle) =
    // Hardware mapping for each element of vector, only do it for bits, valid and ready is broadcast
    hardwareImplementation(
      inputBits,
      inputValids,
      outputReadys,
      Opcode.getOrElse(0.U),
      opDataTypeSet,
      opcodeMapping,
      clock = clock,
      reset = reset
    )

  // Pass downward the bits and valid
  require(
    Results.length == outputBits.length,
    s"You defined ${Results.length} results for FU, " +
      s"but hardware function produces ${outputBits.length} results"
  )
  Results.zip(outputBits.zip(outputValids)).foreach { case (result, (bits, valid)) =>
    result.bits := bits; result.valid := valid
  }

  // Pass upward the ready signal
  require(
    Operands.length == inputReadys.length,
    s"You defined ${Operands.length} operands for FU, " +
      s"but hardware function produces ${inputReadys.length} readies for operands"
  )
  Operands.zip(inputReadys).foreach { case (operand, ready) => operand.ready := ready }

  /* Reduce the Exception and connection to output*/
  EnumBundle.connect(Exception, outputException)
}
