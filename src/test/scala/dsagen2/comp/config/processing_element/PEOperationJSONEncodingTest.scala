package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.CompKeys.DsaOperations
import dsagen2.top.config.JsonParsableKey
import dsagen2.top.config.operation.DataType.{Double64, SignedInt64}
import dsagen2.top.config.operation.Operation.{FixedAdd, FixedMul, FloatAdd, FloatMul}
import dsagen2.util.CDE.cde2json
import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json.{JsValue, Json}

/**
 * JSON emit and parse should not mess up the order (since we use the order as operation encoding)
 */
class PEOperationJSONEncodingTest extends AnyFlatSpec {
  implicit val p : Parameters =
    new WithDsaOperations(Set(FixedAdd + SignedInt64, FixedMul + SignedInt64, FloatAdd + Double64, FloatMul + Double64))
  val key : JsonParsableKey = DsaOperations
  "JSON emit and parse" should "not mess up the relative order of operation" in {
    // Get the operation to encoding mapping
    val mapping :Map[String, Int] = p(DsaOperations).getDsaOpDataTypeString2EncMap
    // mapping.toSeq.sortWith{case ((_, e1), (_, e2)) => e1 < e2} foreach println
    // Get the sequence of operation string and how they are dumped
    val operations : Seq[String] = mapping.toSeq.sortWith{case ((_, e1), (_, e2)) => e1 < e2}.map(_._1)
    // Get the json string of selected string
    val jsonStr : String = Json.prettyPrint(cde2json(Seq(key)))
    // Parse back the emitted String
    val jsonValue : JsValue = Json.parse(jsonStr)
    val parsedOperations : Seq[String] = (jsonValue \ key.getClass.getName \ "OperationDataTypeSet").as[Array[String]]
    // Operations String should be identical
    require(operations.length == parsedOperations.length)
    operations.zip(parsedOperations).foreach{
      case (str, str1) => require(str == str1, s"$str != $str1")
    }
    // Print the operation sequence in Json
    operations foreach println
  }
}
