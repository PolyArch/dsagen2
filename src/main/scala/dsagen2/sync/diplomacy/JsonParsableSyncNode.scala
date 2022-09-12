package dsagen2.sync.diplomacy

import chipsalliance.rocketchip.config.Parameters
import dsagen2.sync.config.SyncKeys.{IVPNode, OVPNode}
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, InputVectorPort, OutputVectorPort}
import dsagen2.util.CDE.cde2FullJson
import dsagen2.util.NodeUtil.failGen
import play.api.libs.json.JsObject

trait JsonParsableSyncNode {
  // Parameter
  implicit val p : Parameters
  val nodeType : DSAGenNodeType
  def numInput : Int
  def numOutput : Int

  // Convert Compute Node to JSON Object
  def toJSON : JsObject = {
    nodeType match {
      case InputVectorPort => cde2FullJson(Seq(IVPNode), Nil, numInput, numOutput)
      case OutputVectorPort => cde2FullJson(Seq(OVPNode), Nil, numInput, numOutput)
      case t : DSAGenNodeType => failGen(t);JsObject.empty
    }
  }
}
