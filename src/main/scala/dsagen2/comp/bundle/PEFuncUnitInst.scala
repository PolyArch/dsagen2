package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.config.CompKeys.{DsaOperations, MetaControl, RegFile}
import dsagen2.comp.config.processing_element.PEDsaOperationParameters
import dsagen2.top.config.DSAFixedConfig.CTRL_MODE_BITS

class PEFuncUnitInst(val numInput: Int, val numOutput: Int)(implicit val p: Parameters) extends Bundle {

  // Extract the parameter
  val aluParam: PEDsaOperationParameters = p.lift(DsaOperations) match {
    case Some(value) => value
    case None        => require(requirement = false, s"ALU Parameter is not found"); p(DsaOperations)
  }
  val numReg: Int = p.lift(RegFile) match {
    case Some(value) => value.numReg
    case None        => 0
  }
  val (inputCtrl, outputCtrl): (Boolean, Boolean) = p.lift(MetaControl) match {
    case Some(value) => (value.inputCtrl, value.outputCtrl)
    case None        => (false, false)
  }
  val maxNumOperand: Int = aluParam.maxNumOperand
  val maxFIFODelay:  Int = aluParam.maxFifoDepth
  val numOpcode:     Int = aluParam.numOpcode
  val maxNumResult:  Int = aluParam.maxNumResult
  val instSlotSize:  Int = aluParam.instSlotSize
  val maxInstDelay:  Int = aluParam.definedLatency
  val maxInstRepeat: Int = aluParam.maxInstRepeatTime

  // Requirement
  if (inputCtrl) require(numInput > 1, s"You support input control, but you only have $numInput inputs")
  require(maxNumOperand > 0, s"non positive $maxNumOperand number of operands?")
  require(numOpcode > 0, s"non positive $maxNumOperand number of opcode?")
  require(maxNumResult > 0, s"You ALU produces no or negative $maxNumResult number of result?")

  // Operands can comes from input ports or register
  def operandSelBits: Int = log2Ceil(numInput + numReg + 1)

  // input control signal can only comes from input ports
  def inputCtrlSelBits: Int = log2Ceil(numInput + 1)

  // Delay Selection Bits
  def delaySelBits: Int = log2Ceil(maxFIFODelay + 1)

  // Opcode selection
  def opcodeBits: Int = log2Ceil(numOpcode)

  // Arithmetic result can goes to output port
  // please be attention here, do not write to port is different from write to port but discarded
  def outputSelBits: Int = log2Ceil(numOutput + 1)

  // Arithmetic result can goes to register
  // please be attention here, do not write to register is different from write to register but reset
  def outRegSelBits: Int = if (numReg > 0) log2Ceil(numReg + 1) else 0

  // Hardware

  // Repeat time of instruction
  val instRepeat: Option[UInt] =
    if (instSlotSize > 1 && maxInstRepeat > 0) Some(UInt(log2Ceil(maxInstRepeat + 1).W)) else None

  // Delay of this instruction, zero delay means combination ALU
  val instDelay: Option[UInt] =
    if (maxInstDelay > 0) Some(UInt(log2Ceil(maxInstDelay + 1).W)) else None

  // Result output selection : register
  val resRegSel: Option[Vec[UInt]] =
    if (outRegSelBits > 0) Some(Vec(maxNumResult, UInt(outRegSelBits.W))) else None

  // Results output selection : output
  val resOutSel: Option[Vec[UInt]] =
    if (outputSelBits > 0) Some(Vec(maxNumResult, UInt(outputSelBits.W))) else None

  // Operation performed
  val fuOpcode: Option[UInt] = if (opcodeBits > 0) Some(UInt(opcodeBits.W)) else None

  // Delay Selection for each delay fifo
  val delayCount: Option[Vec[UInt]] =
    if (delaySelBits > 0 && !aluParam.isDynamic) Some(Vec(maxNumOperand, UInt(delaySelBits.W))) else None

  // If input control is enabled, select from one of input port of as key to access LUT
  // At least two input ports are needed for input controlled instruction
  val inputCtrlSel: Option[UInt] =
    if (inputCtrl && numInput > 1) Some(UInt(inputCtrlSelBits.W)) else None

  // Control Mode (2-bit)
  val ctrlMode: Option[UInt] =
    if (inputCtrl || outputCtrl) Some(UInt(CTRL_MODE_BITS.W)) else None

  // Operands selection between input and register files
  val operSels: Option[Vec[UInt]] =
    if (operandSelBits > 0) Some(Vec(maxNumOperand, UInt(operandSelBits.W))) else None

  // Valid
  val valid: Bool = Bool()
}
