package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3.{Bool, Clock, Reset, UInt}
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.comp.impl.proto.{ASICParameter, FPGAParameter, XilinxVU9PFpga}
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.DsaDataType
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet.setOpDataTypeEnhance
import dsagen2.top.config.operation.Operation.DsaOperation

import scala.collection.Set

trait IsFunctionUnitImplementation {

  // Hidden Variable for sanity check
  val supportOperation: Set[DsaOperation]
  val supportDataType:  Set[DsaDataType]

  // Software related performance number
  val latency:    Int
  val throughput: Int

  // Function to check the actual implementation backend of Function unit
  def isFPGA()(implicit p: Parameters): Boolean = p(FPGAOverlay)

  // A interface for evaluation of FPGA/ASIC's Performance-Power-Area (PPA)
  // These two functions should be overwritten by actual Function Unit Implementation
  def asic: ASICParameter = ASICParameter(28, "UMC", this)

  def fpga: FPGAParameter = FPGAParameter(XilinxVU9PFpga, this)

  // Floating Point Unitily
  def exp(f: Int): Int = f match {
    case 8  => 4;
    case 16 => 5;
    case 32 => 8;
    case 64 => 11;
    case _  => require(false); -1
  }

  def sig(f: Int): Int = f match {
    case 8  => 4;
    case 16 => 11;
    case 32 => 24;
    case 64 => 53;
    case _  => require(false); -1
  }

  /** Initial sanity check
    *
    * @param inputBits    Bits for all input operands
    * @param inputValids  Valids for all input operands
    * @param outputReadys Readys for all output results
    */
  def sanity(
    inputBits:     Seq[UInt],
    inputValids:   Seq[Bool],
    outputReadys:  Seq[Bool],
    opDataTypeSet: Set[DsaOperDataType]
  ): Unit = {
    val maxNumOperand: Int = opDataTypeSet.maxNumOperand
    val maxNumResult:  Int = opDataTypeSet.maxNumResult
    // Check the #input/#output to equal the number of max(#operand) and max(#result)
    require(
      inputBits.length == maxNumOperand &&
        inputBits.length == inputValids.length &&
        outputReadys.length == maxNumResult,
      s"$opDataTypeSet needs $maxNumOperand inputs and $maxNumResult, " +
        s"but we only have ${inputBits.length} bits, ${inputValids.length} valids, " +
        s"and ${outputReadys.length} output readys"
    )
    // Check the set of operation+datatype is supported
    opDataTypeSet.foreach { opDataType: DsaOperDataType =>
      opDataType.dataType match {
        case Some(dT) =>
          require(
            supportOperation.contains(opDataType.operation) && supportDataType.contains(dT),
            s"Operation ${opDataType.operation} and its Data Type $dT is not supported by ${this.getClass.getSimpleName}"
          )
        case None => // bit wise operation
          require(supportOperation.contains(opDataType.operation), "")
      }
    }
  }

  /** Return Wrapper to do sanity check
    *
    * @param inputBits    Bits for all input operands
    * @param inputValids  Valids for all input operands
    * @param outputReadys Readys for all output results
    * @param outputBits   Bits for all output results
    * @param outputValids Valids for all output results
    * @param inputReadys  Readys for all input operands
    * @return pass through output bits, input
    */
  def callback(
    inputBits:    Seq[UInt],
    inputValids:  Seq[Bool],
    outputReadys: Seq[Bool],
    outputBits:   Seq[UInt],
    outputValids: Seq[Bool],
    inputReadys:  Seq[Bool],
    exception:    EnumBundle
  ): (Seq[UInt], Seq[Bool], Seq[Bool], EnumBundle) = {
    // Number of #IO should be consistent for bits/valid/ready
    require(
      outputBits.length == outputValids.length,
      s"OutputBits[${outputBits.length}] != OutputValids[${outputValids.length}]"
    )
    require(
      outputBits.length == outputReadys.length,
      s"OutputBits[${outputBits.length}] != OutputReadys[${outputReadys.length}]"
    )
    require(
      inputBits.length == inputValids.length,
      s"InputBits[${inputBits.length}] != InputValids[${inputValids.length}]"
    )
    require(
      inputBits.length == inputReadys.length,
      s"InputBits[${inputBits.length}] != InputReady[${inputReadys.length}]"
    )
    // Homogeneous Operand/Result bits
    val seqInputBits:  Seq[Int] = inputBits.map(_.getWidth).distinct
    val seqOutputBits: Seq[Int] = outputBits.map(_.getWidth).distinct
    require(seqInputBits.length <= 1, "Heterogeneous Input Bits")
    require(seqOutputBits.length <= 1, "Heterogeneous Output Bits")
    // Operation should not change data type
    val numInputBits:  Int = if (seqInputBits.length == 1) inputBits.map(_.getWidth).distinct.head else 0
    val numOutputBits: Int = if (seqOutputBits.length == 1) outputBits.map(_.getWidth).distinct.head else 0
    if (seqInputBits.nonEmpty && seqOutputBits.nonEmpty)
      require(numInputBits == numOutputBits, s"Input[${numInputBits - 1}:0] -> Output[${numOutputBits - 1}:0]")
    // Return
    (outputBits, outputValids, inputReadys, exception)
  }

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
    opDataTypeSet: Set[DsaOperDataType],
    encoding:      Map[DsaOperDataType, Int],
    clock:         Clock,
    reset:         Reset
  )(
    implicit p: Parameters
  ): (Seq[UInt], Seq[Bool], Seq[Bool], EnumBundle)

  /** Apply hardware arithmetic implementation to input/output bits/valid/ready with Opcode and return exception
    *
    * @param inputBits     Bits for all input port
    * @param inputValids   Valid for all input port
    * @param outputReadys  Ready for all output port
    * @param opcode        Opcode wire from ALU
    * @param opDataTypeSet The Set of Operation+DataType assigned to this function unit
    * @param encoding      The encoding mapping from Operation+DataType to opcode value
    * @param p             implicit parameter of Processing Element
    * @return output bits/valid, input ready, exception
    */
  def apply(
    inputBits:     Seq[UInt],
    inputValids:   Seq[Bool],
    outputReadys:  Seq[Bool],
    opcode:        UInt,
    opDataTypeSet: Set[DsaOperDataType],
    encoding:      Map[DsaOperDataType, Int],
    clock:         Clock,
    reset:         Reset
  )(
    implicit p: Parameters
  ): (Seq[UInt], Seq[Bool], Seq[Bool], EnumBundle) = {
    // Initial Sanity Check
    sanity(inputBits, inputValids, outputReadys, opDataTypeSet)
    // Apply Real Hardware Implementation
    val (outputBits, outputValids, inputReadys, exceptions) =
      implement(inputBits, inputValids, outputReadys, opcode, opDataTypeSet, encoding, clock, reset)
    // Return result check
    callback(inputBits, inputValids, outputReadys, outputBits, outputValids, inputReadys, exceptions)
  }
}

object IsFunctionUnitImplementation {
  val implPackageName: String = classOf[IsFunctionUnitImplementation].getPackage.getName
  val allImpls: Set[IsFunctionUnitImplementation] =
    dsagen2.util.AppUtil.subObjects(implPackageName, classOf[IsFunctionUnitImplementation]).toSet
}
