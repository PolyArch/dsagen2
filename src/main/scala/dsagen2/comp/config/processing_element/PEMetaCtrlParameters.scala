package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.CompKeys.{DsaOperations, RegFile}
import dsagen2.comp.config.ReconfParameters
import dsagen2.util.BooleanUtil.boolean2int
import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json.{JsValue, Json, Writes}

case class PEMetaCtrlParameters( // Input/Output Controlled
  inputCtrl:  Boolean = true,
  outputCtrl: Boolean = true,
  // Meta Controlled Function
  abstain:       Boolean = true,
  resetReg:      Boolean = true,
  reuseOperand:  Boolean = true,
  discardResult: Boolean = true,
  // Size of Meta LUT
  sizeLUT: Int = 4)
    extends ReconfParameters
    with JSONParsableParameters {
  // Derived parameter
  def keyBits: Int = 6 // For now, only stream state (6-bit) and compare result (3-bit) can be used, take the maximum

  // Whether it is meta controlled
  def supportControl: Boolean =
    (inputCtrl || outputCtrl) && (resetReg || reuseOperand || discardResult)

  // require
  if (supportControl) require(keyBits >= 0 && sizeLUT > 0)

  // How many meta controlled it support
  def numMetaCtrlMode: Int = inputCtrl + outputCtrl

  // Get the number of resettable register
  def numResetRegister(implicit p: Parameters): Int = p.lift(RegFile) match {
    case Some(regParam) =>
      regParam.numResetReg
    case None =>
      if (resetReg) {
        require(
          requirement = false,
          s"You support reset register, but RegisterFile parameter cannot be found. " +
            s"Maybe you did not chain parameter in the right order"
        )
      }
      0
  }

  // How many bits required for meta control
  // Meta Control CSR does not require input/output parameters
  def csrBits(implicit p: Parameters): Int =
    csrFieldBits().map(_._2).sum

  // Get the Meta Control Field and Bits
  // Meta Control CSR has nothing to do with input / output, but to keep the function type same, we keep it
  def csrFieldBits(num_input: Int = 0, num_output: Int = 0)(implicit p: Parameters): Seq[(String, Int)] = {
    // Get the ALU related info from CDE
    val aluParam: Option[PEDsaOperationParameters] = p.lift(DsaOperations)
    if (reuseOperand || discardResult) {
      require(aluParam.isDefined, s"You are doing reuse or discard but ALU parameter cannot be found")
    }

    // Field and CSR Bits per Entry
    val entryBits: Seq[(String, Int)] =
      (0 until sizeLUT).flatMap { entryIdx =>
        // Generate the optional field
        val aAll = if (abstain) Seq((s"${entryIdx}_abstain", 1)) else Nil
        val sReg = if (resetReg) Seq((s"${entryIdx}_resetReg", numResetRegister)) else Nil
        val rOpd = if (reuseOperand) Seq((s"${entryIdx}_reuseOperand", aluParam.get.maxNumOperand)) else Nil
        val dRes = if (discardResult) Seq((s"${entryIdx}_discardResult", aluParam.get.maxNumResult)) else Nil
        // Encode Control Field     LSB :   reuse   discard  reset   abstain  : MSB
        val optField: Seq[(String, Int)] = rOpd ++ dRes ++ sReg ++ aAll
        // Generate mandatory field
        val validField: Seq[(String, Int)] = Seq(
          (s"${entryIdx}_valid", 1)
        )
        // Return
        validField ++ optField
      }
    val prefixEntryBits: Seq[(String, Int)] = entryBits.map(x => ("MetaCtrlEntry_" + x._1, x._2))
    // Return
    if (supportControl) prefixEntryBits else Nil
  }

  /* ---------- JSON Emitter ---------- */
  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[PEMetaCtrlParameters] {
    def writes(o: PEMetaCtrlParameters): JsValue = json.deepMerge(
      Json.obj(
        fields =
          // Dump Support Control Mode
          "inputLSBCtrl" -> o.inputCtrl,
        "outputLSBCtrl" -> o.outputCtrl,
        // Control Behaviour
        "abstain" -> o.abstain,
        "resetRegister" -> o.resetReg,
        "reuseOperand" -> o.reuseOperand,
        "discardResult" -> o.discardResult,
        // Meta Control LUT properties
        "sizeLUT" -> o.sizeLUT
      )
    )
  }
}

object PEMetaCtrlParameters extends JSONParsableConstructor {
  def apply(json: JsValue): PEMetaCtrlParameters =
    PEMetaCtrlParameters(
      // Input/Output Controlled
      inputCtrl = (json \ "inputLSBCtrl").as[Boolean],
      outputCtrl = (json \ "outputLSBCtrl").as[Boolean],
      // Meta Controlled Function
      abstain = (json \ "abstain").as[Boolean],
      resetReg = (json \ "resetRegister").as[Boolean],
      reuseOperand = (json \ "reuseOperand").as[Boolean],
      discardResult = (json \ "discardResult").as[Boolean],
      // Size of Meta LUT
      sizeLUT = (json \ "sizeLUT").as[Int]
    )
}
