package dsagen2.comp.config

import dsagen2.top.diplomacy.DSANodeType.ProcessingElement
import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json.Json

class SingleCompNodeJsonParserTest extends AnyFlatSpec {
  "A JSON file" should "be parsed as CompNodeParameters" in {
    val jsonStr : String = scala.io.Source.fromResource("jsonParse.json").getLines().mkString
    val nodePara : CompNodeParameters = CompNodeParameters(Json.parse(jsonStr))
    val parseNodeJson : String = Json.prettyPrint(nodePara.toJSON)
    //println(parseNodeJson)
  }

  val node : CompNodeParameters = CompNodeParameters(
    nodeId = 0,
    nodeType = ProcessingElement,
    compUnitBits = 16,
    compBits = 32)
  "A Comp Node parameter" should "be printed as JSON" in {
    val jsonStr : String = node.emitJSON
    //println("Comp Node emitted JSON : \n" + jsonStr)
  }
  it should "read emitted JSON as CompNodeParameters" in {
    val nodeByJSON : CompNodeParameters = CompNodeParameters(Json.parse(node.emitJSON))
    require(node.getNodeId == nodeByJSON.getNodeId)
    require(node.nodeType == nodeByJSON.nodeType)
    require(node.supportNodeActive == nodeByJSON.supportNodeActive)
    require(node.compBits == nodeByJSON.compBits )
    require(node.compUnitBits == nodeByJSON.compUnitBits)
  }
}