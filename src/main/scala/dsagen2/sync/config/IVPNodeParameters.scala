package dsagen2.sync.config

import dsagen2.top.config.enumeration.VPImplMode._
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, InputVectorPort}
import dsagen2.top.diplomacy.WithNodeIDParameters
import dsagen2.top.module.DSAGen
import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json.{JsNumber, JsObject, JsValue, Json, Writes}

/** Basic node parameter for input vector port. The vectorization width is defined by topology
  *
  * @param nodeId       Node ID
  * @param nodeType     Node Type of the input vector port
  * @param depthByte    the depth of vector port in unit of byte
  * @param vpStated     whether this input vector port produces stream state at highest position
  *                     with overhead of one extra port. If enabled, each unit of vector port will be appended with
  *                     an 6-bit stream state. The extra port will the the bitOr of stream state of all units
  * @param repeatedIVP  whether this input vector port can do stream repeated FSM. If it is enabled, the vector port
  *                     can repeat the vector dequeue in a repeated way
  * @param broadcastIVP whether this input vector port can broadcast stream. If it is enabled, when broadcast bit is set,
  *                     This vector port will be feed with same data unit with the target port.
  */
case class IVPNodeParameters( // Node Identification
  nodeId:   Int = -1, // Assigned during generation
  nodeType: DSAGenNodeType = InputVectorPort,
  // Data Granularity
  depthByte: Int = 2, // 2 means minimal depth
  // Common Vector Port Function
  vpImpl:   VPImpl = NonXBarVP, // FU
  vpStated: Boolean = true,
  // Input Vector Port Specific Function
  repeatedIVP:  Boolean = true,
  broadcastIVP: Boolean = true,
  // Comp Node Parameters Carried for Memory Read Bundle Negotiation
  compNodesBits: Seq[Int] = Nil // Defined by ADG, will not be dumped by JSON
) extends VPNodeParameters {

  // Direction
  def isInput: Boolean = true

  // Parameter emit to JSON object
  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[IVPNodeParameters] {
    def writes(ivpNode: IVPNodeParameters): JsObject = json.deepMerge(
      Json.obj(
        fields = // Node ID
          "nodeId" -> ivpNode.getNodeId,
        "nodeType" -> ivpNode.nodeType,
        // Data Granularity
        "depthByte" -> ivpNode.depthByte,
        // Common Vector Port Properties
        "vpImpl" -> ivpNode.vpImpl.id,
        "vpStated" -> ivpNode.vpStated,
        // Input Vector Port Specific Properties
        "repeatedIVP" -> ivpNode.repeatedIVP,
        "broadcastIVP" -> ivpNode.broadcastIVP
      )
    )
  }
}

// Input vector port parameter constructor from JSON
object IVPNodeParameters extends JSONParsableConstructor {
  // Convert JSON Object to Input Vector Node Parameters
  def apply(json: JsValue): IVPNodeParameters = {
    IVPNodeParameters(
      // Node ID
      nodeId = (json \ "nodeId").getOrElse(JsNumber(-1)).as[Int],
      nodeType = (json \ "nodeType").as[String],
      // Data Granularity
      depthByte = (json \ "depthByte").as[Int],
      // Common Vector Port Properties
      vpImpl = (json \ "vpImpl").as[Int],
      vpStated = (json \ "vpStated").as[Boolean],
      // Input Vector Port Specific Properties
      repeatedIVP = (json \ "repeatedIVP").as[Boolean],
      broadcastIVP = (json \ "broadcastIVP").as[Boolean]
    )
  }
}
