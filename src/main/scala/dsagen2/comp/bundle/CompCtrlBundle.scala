package dsagen2.comp.bundle

import chisel3._
import dsagen2.comp.config.CompNodeParameters

/** This bundle describe the data structure of CGRA Compute Control Field
  *
  * @param compNode Compute Node Parameter
  */
class CompCtrlBundle(compNode: CompNodeParameters) extends Bundle {
  // Self node activity status
  val dAct: Option[Bool] = if (compNode.supportNodeActive) {
    Some(Output(Bool()))
  } else None

  // The other node activity status
  val uAct: Option[Bool] = if (compNode.supportNodeActive) {
    Some(Input(Bool()))
  } else None
}
