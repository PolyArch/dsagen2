package dsagen2.mem.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.top.diplomacy.DSANodeType.DSAGenNodeType

class StreamROBEntry(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit val p: Parameters)
    extends Bundle {
  require(memNode.hasStrRequest, s"Only DMA or SPM need stream request")

  /* ---------- Derived Parameters ---------- */
  val nodeType: DSAGenNodeType = memNode.nodeType

  /* ---------- Hardware Fields ---------- */

  // Stream Meta Info (Short Version of Stream Entry)
  val meta: StreamMeta = new StreamMeta(memNode, ivpsParam, ovpsParam, hasMStatus = false)

  // Stream State: The current state of stream
  val state: Option[StreamState] =
    if (memNode.streamStated) Some(new StreamState()) else None

  // Mask
  val expectMask: UInt = UInt(memNode.bandwidth.W)
}
