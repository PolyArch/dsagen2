package dsagen2.comp.diplomacy

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.CompNodeParameters

class CompDirEdgeParameters(val enable: Boolean = true, val compNode: CompNodeParameters, p: Parameters) {
  override def hashCode(): Int =
    enable.hashCode() * 11 + compNode.hashCode() * 19 + p.hashCode() * 23

  override def equals(obj: Any): Boolean =
    obj match {
      case dirEdge: CompDirEdgeParameters => this.hashCode() == dirEdge.hashCode()
      case _ => false
    }
}
