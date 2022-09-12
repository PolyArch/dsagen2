package dsagen2.comp.config

import chipsalliance.rocketchip.config._
import dsagen2.comp.config.CompKeys._
import dsagen2.comp.config.processing_element.DefaultPEConfig
import dsagen2.top.config.JsonParsableKey
import dsagen2.util.CDE.{cde2json, json2cde}
import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json.Json


class CDEJsonParserTest extends AnyFlatSpec {
  implicit val p : Parameters = new DefaultPEConfig(0)
  val keys : Seq[JsonParsableKey] = PEKeys.allKeys

  "CDE Json Parser" should "convert CDE parameter to JSON" in {
    val cdeJsonString : String = Json.prettyPrint(cde2json(keys))
    //println("Original CDE -> JSON : ")
    //println(cdeJsonString)
  }

  it should "convert JSON string to CDE parameter" in {
    val jsonStr : String = Json.prettyPrint(cde2json(keys))
    val parsedParameter : Parameters = json2cde(Json.parse(jsonStr))
    val jsonStringFromParsed : String = Json.prettyPrint(cde2json(keys)(parsedParameter))
    //println("Original CDE -> JSON -> Parsed CDE -> JSON: ")
    //println(jsonStringFromParsed)
    require(
      Json.prettyPrint(cde2json(keys)(parsedParameter)) ==
        Json.prettyPrint(cde2json(keys)(p)),
      "Passing through JSON parser should not change dumped JSON"
    )
  }
}
