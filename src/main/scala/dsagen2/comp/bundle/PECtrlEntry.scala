package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.comp.config.CompKeys.{DsaOperations, MetaControl, RegFile}
import dsagen2.comp.config.processing_element.{PEDsaOperationParameters, PEMetaCtrlParameters, PERegFileParameters}

/** Processing Element's Meta Control Entry
  *
  * Example: for Accumulation,
  * C = A + B
  * acc = C
  * output[select] = C
  *
  * disResults = [0, 1]
  * reuseOpers = [1, 1]
  * resetRegs = [1, 0, 1]
  */
class PECtrlEntry(implicit val p: Parameters) extends Bundle {

  // Extract parameter
  val fuParam:   Option[PEDsaOperationParameters] = p.lift(DsaOperations)
  val ctrlParam: Option[PEMetaCtrlParameters] = p.lift(MetaControl)
  val regParam:  Option[PERegFileParameters] = p.lift(RegFile)

  // Utility
  def ctrlExist: Boolean = ctrlParam.isDefined && ctrlParam.get.supportControl

  // Hardware

  // MSB

  // Abstain
  val abstain: Option[Bool] = if (ctrlExist && ctrlParam.get.abstain) Some(Bool()) else None // do nothing

  // Reset Register
  val regReset: Option[UInt] =
    if (ctrlExist && regParam.isDefined && ctrlParam.get.resetReg && regParam.get.numResetReg > 0)
      Some(UInt(regParam.get.numResetReg.W))
    else None // asBools

  // Discard Result
  val resultDis: Option[UInt] =
    if (ctrlExist && fuParam.isDefined && ctrlParam.get.discardResult && fuParam.get.maxNumResult > 0)
      Some(UInt(fuParam.get.maxNumResult.W))
    else None // asBools

  // Reuse Operands
  val operReuse: Option[UInt] =
    if (ctrlExist && fuParam.isDefined && ctrlParam.get.reuseOperand && fuParam.get.maxNumOperand > 0)
      Some(UInt(fuParam.get.maxNumOperand.W))
    else None // asBools

  // Valid
  val valid: Option[Bool] = if (ctrlExist) Some(Bool()) else None // valid entry

  // LSB
}
