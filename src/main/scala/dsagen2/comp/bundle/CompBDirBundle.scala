package dsagen2.comp.bundle

import chisel3._
import dsagen2.comp.diplomacy.CompBDirEdgeParameters

/** This is the actual bundle among compute node OR between sync node and compute node.
  *
  * This is the BI/BO
  *
  * @param ep The bi-directional edge parameters
  */
class CompBDirBundle(val ep: CompBDirEdgeParameters) extends Bundle {

  // Sanity check: For this bi-directional edge,
  require(ep.dParam.enable || ep.uParam.enable, s"Connection must be at lease one directional, cannot be nothing")

  // The Actual Hardware

  // Downward Bundle
  val dPort: Option[CompDirBundle] = if (ep.dParam.enable) Some(new CompDirBundle(ep.dParam)) else None

  // Upward Bundle
  val uPort: Option[CompDirBundle] = if (ep.uParam.enable) Some(Flipped(new CompDirBundle(ep.uParam))) else None
}
