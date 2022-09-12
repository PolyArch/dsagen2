package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.comp.config.CompKeys.CompNode
import dsagen2.comp.diplomacy.CompDirEdgeParameters

/** This field describe the data structure of directional connection between two nodes of Compute system
  *
  * valid: packet valid
  * ready: packet is accepted
  * ctrl: control field [CompCtrlBundle]
  * data: data field [CompDataBundle]
  *
  * @param param Directional Edge Parameter
  */
class CompDirBundle(val param: CompDirEdgeParameters) extends Bundle {

  // Extract parameters
  implicit val p: Parameters = Parameters.empty.alterPartial({ case CompNode => param.compNode })

  // Actual Hardware, Ready, Valid, Ctrl, Data
  val ready: Option[Bool] = if (param.enable) Some(Input(Bool())) else None
  val valid: Option[Bool] = if (param.enable) Some(Output(Bool())) else None
  val ctrl:  Option[CompCtrlBundle] = if (param.enable) Some(new CompCtrlBundle(param.compNode)) else None
  val data:  Option[CompDataBundle] = if (param.enable) Some(new CompDataBundle(param.compNode)) else None
}
