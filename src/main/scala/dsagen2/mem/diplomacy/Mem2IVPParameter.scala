package dsagen2.mem.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}
import chisel3.util.log2Ceil
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.IVPNodeParameters

/** This is the actual place that memory node <-> input vector port got negotiated
  * The negotiated parameter will directly be used to generate Memory Read Response Bundle
  *
  * These are the parameters that need to be negotiated:
  *  1. how wide for this memory unit vector?
  *     2. how many bits for each memory unit
  *     3. stream related parameter like: stream state
  *     TODO: for now, stream state is full set, we should fix it later. It should not always be full set, we should discuss.
  *
  * @param memNode    Parameter from memory node to input vector port
  * @param ivpNode    parameter from input vector port to memory node
  * @param p          CDE parameter
  * @param sourceInfo chisel source code info
  */
case class Mem2IVPParameter(
  memNode:    MemNodeParameters,
  ivpNode:    IVPNodeParameters,
  p:          Parameters = Parameters.empty,
  sourceInfo: SourceInfo = UnlocatableSourceInfo) {
  // Negotiation the depth bits
  def depthBits: Int = {
    val (sumCompBits, minCompBits): (Int, Int) = if (ivpNode.compNodesBits.nonEmpty) {
      // TODO: I will fix it later
      if (false)
        println(
          s"WARNING: IVP is not connected to any compute node, please " +
            s"connect to compute node first, then connect it to memory. Otherwise, program cannot " +
            s"determine the depth of IVP"
        )
      (ivpNode.compNodesBits.sum, ivpNode.compNodesBits.min)
    } else (0, Int.MaxValue)

    /* 256 means that depth Bits is at least 8 bit */
    val minDepth: Int = (sumCompBits + memNode.bandBits) / (memNode.memUnitBits.min(minCompBits))
    8.max(log2Ceil(1 << log2Ceil(minDepth)))
  }

  // Generate the label for the edge
  // for connection between memory node to input vector port, it is always single directional
  def edgeLabel: String = memNode.getNodeName + "-&gt;" + ivpNode.getNodeName

  // Overwrite hashcode for equality check
  override def hashCode(): Int =
    memNode.hashCode() * 11 + ivpNode.hashCode() * 19 + p.hashCode() * 23

  override def equals(obj: Any): Boolean =
    obj match {
      case dirEdge: Mem2IVPParameter => this.hashCode() == dirEdge.hashCode()
      case _ => false
    }
}
