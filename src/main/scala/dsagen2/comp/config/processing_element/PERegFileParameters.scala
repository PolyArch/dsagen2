package dsagen2.comp.config.processing_element

import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json.{JsValue, Writes}

/** The parameter of the register file of Processing Element
  *
  * @param numReg      the number of resgiter of Processing element
  * @param asyncRF     Register File is Async Register File, which is able to read the written value in same cycle
  * @param update      Register File can be updated from config port
  * @param resetRegIdx The set of resettable register number
  */
case class PERegFileParameters(
  numReg:      Int = 1, // User defined
  asyncRF:     Boolean = true, // sync or async
  update:      Boolean = true, // update from config port
  resetRegIdx: Seq[Int] = Seq(0) // by default the first register is resettable
) extends JSONParsableParameters {

  // Sanity Check
  resetRegIdx.foreach { regIdx =>
    require(
      regIdx < numReg,
      s"The register file has in total $numReg register, " +
        s"but you specify the reg[$regIdx] as resettable"
    )
  }

  // The number of resettable number
  def numResetReg: Int = resetRegIdx.size

  import play.api.libs.json.Json

  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[PERegFileParameters] {
    override def writes(o: PERegFileParameters): JsValue = json.deepMerge(
      Json.obj(fields = "numReg" -> numReg, "asyncRF" -> asyncRF, "update" -> update, "resetRegIdx" -> resetRegIdx)
    )
  }
}

// Parameters Constructor
object PERegFileParameters extends JSONParsableConstructor {
  def apply(json: JsValue): PERegFileParameters =
    PERegFileParameters(
      numReg = (json \ "numReg").as[Int],
      asyncRF = (json \ "asyncRF").as[Boolean],
      update = (json \ "update").as[Boolean],
      resetRegIdx = (json \ "resetRegIdx").as[Seq[Int]]
    )
}
