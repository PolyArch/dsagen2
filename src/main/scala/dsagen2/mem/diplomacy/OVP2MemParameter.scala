package dsagen2.mem.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}
import chisel3.util.log2Ceil
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.OVPNodeParameters

/** The edge parameter from output vector port to memory node
  *
  * @param ovpNode    parameter downward : output vector port parameter
  * @param memNode    parameter upward : memory write parameter
  * @param p          CDE parameter
  * @param sourceInfo chisel source info
  */
case class OVP2MemParameter(
  ovpNode:    OVPNodeParameters,
  memNode:    MemNodeParameters,
  p:          Parameters = Parameters.empty,
  sourceInfo: SourceInfo = UnlocatableSourceInfo) {

  // Negotiation the depth bits
  def depthBits: Int = {
    val (sumCompBits, minCompBits): (Int, Int) = if (ovpNode.compNodesBits.nonEmpty) {
      // TODO: I will fix it later
      if (false)
        println(
          s"OVP is not connected to any compute node, please " +
            s"connect to compute node first, then connect it to memory. Otherwise, program cannot " +
            s"determine the depth of OVP"
        )
      (ovpNode.compNodesBits.sum, ovpNode.compNodesBits.min)
    } else (0, Int.MaxValue)
    val minDepth: Int = (sumCompBits + memNode.bandBits) / (memNode.memUnitBits.min(minCompBits))
    log2Ceil(1 << log2Ceil(minDepth)).max(8)
  }

  // The edge label for the connection from output vector port to memory node
  def edgeLabel: String = ovpNode.getNodeName + "-&gt;" + memNode.getNodeName

  // Overwrite hashcode for equality
  override def hashCode(): Int =
    ovpNode.hashCode() * 11 + memNode.hashCode() * 19 + p.hashCode() * 23

  override def equals(obj: Any): Boolean =
    obj match {
      case dirEdge: OVP2MemParameter => this.hashCode() == dirEdge.hashCode()
      case _ => false
    }
}
