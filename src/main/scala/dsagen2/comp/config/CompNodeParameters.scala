package dsagen2.comp.config

import chisel3.util.isPow2
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.top.diplomacy.WithNodeIDParameters
import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json._

/** @param nodeId           Unique ID for Node Parameter, -1 is unset
  * @param nodeType          PE or SW
  * @param compBits          Widest Data Granularity can be processed
  * @param compUnitBits      Minimum Data Granularity can be processed
  * @param supportNodeActive Whether this node tells other node its activity status
  * @param comment           Optional comment which will be appended as part of compute node name
  */
case class CompNodeParameters( // Node ID
  nodeId:   Int = -1,
  nodeType: DSAGenNodeType = Switch,
  // Data Granularity
  compBits:     Int = 64,
  compUnitBits: Int = 8,
  // Node Meta Info
  // node active has to be enabled for compute node connect to ivp
  supportNodeActive: Boolean = true,
  comment:           String = "")
    extends JSONParsableParameters
    with WithNodeIDParameters {

  /* ---------- Requirement ---------- */
  require(compBits % compUnitBits == 0, s"Compute Bits should be able to be evenly divided by compute unit bits")
  require(numCompUnit >= 1, s"CompBits = $compBits, compUnitBits = $compUnitBits, this is not allowed")
  if ((nodeType == ProcessingElement) || (nodeType == Switch)) {
    require(
      isPow2(compBits) && isPow2(compUnitBits) && compBits > 1 && compUnitBits > 1,
      s"Compute Node is $ProcessingElement or $Switch, but nodeType = $nodeType" +
        s"Data Width ($compBits) and Data Unit Width ($compUnitBits) should be power of 2"
    )
  } else if ((nodeType == InputVectorPort) || (nodeType == OutputVectorPort)) {
    require(isDummy, s"Compute Node Parameter used by VP can only be dummy")
  }

  /* ---------- Derived Parameters ---------- */
  // Number of Subnet
  def numCompUnit: Int = {
    compBits / compUnitBits
  }

  // is dummy
  def isDummy: Boolean = compBits == 1 && compUnitBits == 1 && !supportNodeActive

  /* ---------- JSON Emitter ----------*/
  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[CompNodeParameters] {
    def writes(node: CompNodeParameters): JsObject = json.deepMerge(
      Json.obj(
        fields = "nodeId" -> getNodeId,
        "nodeType" -> node.nodeType.toString,
        "supportNodeActive" -> supportNodeActive,
        "compBits" -> node.compBits,
        "compUnitBits" -> node.compUnitBits,
        "comment" -> comment
      )
    )
  }
}

// Compute Node Parameter Constructor
object CompNodeParameters extends JSONParsableConstructor {
  // Convert JSON Object to Compute Node Parameters
  def apply(json: JsValue): CompNodeParameters = {
    CompNodeParameters(
      nodeId = (json \ "nodeId").getOrElse(JsNumber(-1)).as[Int],
      nodeType = (json \ "nodeType").as[String],
      compBits = (json \ "compBits").getOrElse(JsNumber(64)).as[Int],
      compUnitBits = (json \ "compUnitBits").getOrElse(JsNumber(8)).as[Int],
      supportNodeActive = (json \ "supportNodeActive").getOrElse(JsBoolean(true)).as[Boolean],
      comment = (json \ "comment").getOrElse(JsString("")).as[String]
    )
  }

  // Convert DSANodeType to Compute Node Parameters
  def apply(compType: DSAGenNodeType): CompNodeParameters =
    new CompNodeParameters(nodeType = compType.toString)
}
