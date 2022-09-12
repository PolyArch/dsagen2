package dsagen2.comp.diplomacy

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.bundle.CompBDirBundle
import dsagen2.comp.config.CompKeys._
import dsagen2.comp.config.CompNodeParameters
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, ProcessingElement, Switch}
import dsagen2.util.CDE.cde2FullJson
import dsagen2.util.NodeUtil.failGen
import freechips.rocketchip.diplomacy.MixedNode
import play.api.libs.json.JsObject

/** This trait enables a Compute Node to have the capability to emit its properties to an JSON Object. In order to embed the
  * topology into JSON ADG, the number of input and the number of output should be defined in the sub class.
  */
trait JsonParsableCompNode {
  // Parameters
  implicit val p: Parameters
  val compNode:   CompNodeParameters
  val node: MixedNode[
    CompNodeParameters,
    CompNodeParameters,
    CompBDirEdgeParameters,
    CompBDirBundle,
    CompNodeParameters,
    CompNodeParameters,
    CompBDirEdgeParameters,
    CompBDirBundle
  ]

  def numInput: Int

  def numOutput: Int

  // Convert Compute Node to JSON Object
  def toJSON: JsObject = {
    compNode.nodeType match {
      case ProcessingElement => cde2FullJson(PEKeys.allKeys, PEKeys.reconfKeys, numInput, numOutput)
      case Switch            => cde2FullJson(SWKeys.allKeys, SWKeys.reconfKeys, numInput, numOutput)
      case t: DSAGenNodeType => failGen(t); JsObject.empty
    }
  }
}
