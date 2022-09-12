package dsagen2.sync.config

import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, OutputVectorPort}
import dsagen2.top.diplomacy.WithNodeIDParameters
import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json._
import dsagen2.top.config.enumeration.VPImplMode._

/** Basic node parameter for output vector port
  * @param nodeId Node ID
  * @param nodeType Name of output vector port
  * @param depthByte depth in the unit of each element
  * @param vpImpl Vector port implementation
  * @param vpStated State in Stream
  * @param taskOVP Task flow support by output vector port
  * @param compNodesBits Bitwidth for all connected compute nodes
  */
case class OVPNodeParameters( // Node Identification
  nodeId:   Int = -1,
  nodeType: DSAGenNodeType = OutputVectorPort,
  // Data Granularity
  depthByte: Int = 2, // -1 means minimum
  // Meta Function that can be provided by output vector port
  vpImpl:   VPImpl = NonXBarVP,
  vpStated: Boolean = true, // TODO, not yet implemented
  // Output vector port specific parameters
  discardOVP: Boolean = true, // TODO: Discard-able output vector port
  taskOVP:    Boolean = true, // TODO: not yet implemented
  // Comp Node Parameters Carried for Memory Read Bundle Negotiation
  compNodesBits: Seq[Int] = Nil // Defined by ADG, will not be dumped by JSON
) extends VPNodeParameters {

  // Direction: output
  def isInput: Boolean = false

  // Parameter emit to JSON object
  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[OVPNodeParameters] {
    def writes(ovpNode: OVPNodeParameters): JsObject = json.deepMerge(
      Json.obj(
        fields = // Node ID
          "nodeId" -> ovpNode.getNodeId,
        "nodeType" -> ovpNode.nodeType,
        // Data Granularity
        "depthByte" -> ovpNode.depthByte,
        // Common Vector Port Properties
        "vpImpl" -> ovpNode.vpImpl.id,
        "vpStated" -> ovpNode.vpStated,
        // Output Vector Port Specific Properties
        "discardOVP" -> ovpNode.discardOVP,
        "taskOVP" -> ovpNode.taskOVP
      )
    )
  }
}

// Output Vector Port parameters constructor from JSON Object
object OVPNodeParameters extends JSONParsableConstructor {
  // Convert JSON Object to Output Vector Port Node Parameters
  def apply(json: JsValue): OVPNodeParameters = {
    OVPNodeParameters(
      // Node ID
      nodeId = (json \ "nodeId").getOrElse(JsNumber(-1)).as[Int],
      nodeType = (json \ "nodeType").as[String],
      // Data Granularity
      depthByte = (json \ "depthByte").as[Int],
      // Common Vector Port Properties
      vpImpl = (json \ "vpImpl").as[Int],
      vpStated = (json \ "vpStated").getOrElse(JsBoolean(false)).as[Boolean],
      // Output Vector Port Specific Properties
      discardOVP = (json \ "discardOVP").getOrElse(JsBoolean(false)).as[Boolean],
      taskOVP = (json \ "taskOVP").getOrElse(JsBoolean(false)).as[Boolean]
    )
  }
}
