package dsagen2.mem.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}

/** Stream Request Bundle sent to each memory request port
  *
  * Some Meta Info is kept from Stream Entry, some are ignored since they will never be used
  *
  * @param memNode   Memory Node
  * @param ivpsParam IVP Parameters
  * @param ovpsParam OVP Parameters
  * @param p         CDE
  */
class StreamRequest(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit val p: Parameters)
    extends Bundle {
  // Check whether memory can create stream request, DIS is allowed since it will not use it and eventually it will
  // be optimized by Chisel
  require(memNode.hasStrRequest || memNode.isDIS, s"Only DMA, SPM, GEN, REC need stream request")

  /* ---------- Hardware Fields ---------- */

  // Whether this entry is valid
  val valid: Bool = Bool()

  // Stream Meta Info (Short Version of Stream Entry)
  val meta: StreamMeta = new StreamMeta(memNode, ivpsParam, ovpsParam)

  // Stream State: The current state of stream
  val state: Option[StreamState] =
    if (memNode.streamStated) Some(new StreamState()) else None

  // Address Bits
  val addr: Option[UInt] = if (memNode.needAddr) Some(UInt(memNode.addrBits.W)) else None

  // Mask
  val mask: Option[UInt] = if (memNode.hasStrRequest) {
    if (memNode.isDMA || memNode.isGEN || memNode.isREC) Some(UInt(memNode.bandwidth.W))
    else if (memNode.isSPM) Some(UInt(memNode.spmBankWidth.W))
    else None
  } else None

  // Data Type Payload for Write/Atomic Operation Request
  val writeData: Option[UInt] = {
    if (memNode.isDMA || memNode.isDIS || memNode.isREC) Some(UInt(memNode.bandBits.W))
    else if (memNode.isSPM) Some(UInt((memNode.memUnitBits * memNode.spmBankWidth).W))
    else if (memNode.isREG) Some(UInt(memNode.XLEN.W))
    else None
  }
}
