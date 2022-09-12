package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.CompKeys._
import dsagen2.util.CDE.{cde2FullJson, cde2json, json2cde}
import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json.Json

class PEADGEmitterTest extends AnyFlatSpec{
  implicit val p : Parameters = new DefaultPEConfig(0)

  "A PE Config" should "emit with bitstream encoding" in {
    //println("Full Json per PEConfig is : ")
    val fullJson = cde2FullJson(PEKeys.allKeys, PEKeys.reconfKeys, 5, 3)
    //println(Json.prettyPrint(fullJson))
  }
  it should "adding bitstream should not mess up json2cde" in {
    val fullJson = cde2FullJson(PEKeys.allKeys, PEKeys.reconfKeys, 5, 3)
    val fullJsonStr : String = Json.prettyPrint(fullJson)
    // Compare the original string and parsed back string
    val originalJsonStr : String = Json.prettyPrint(cde2json(PEKeys.allKeys))
    val parseBackStr : String = Json.prettyPrint(
      cde2json(PEKeys.allKeys)(json2cde(Json.parse(fullJsonStr)))
    )
    require(originalJsonStr == parseBackStr)
  }
}
