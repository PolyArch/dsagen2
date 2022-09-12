package dsagen2.mem.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.util.UIntUtil.groupBitsAs

/** Stream Response Bundle receive from memory response port, for read only (only read need response right?)
  * Some Meta Info is kept from Stream Entry, some are ignored since they will never be used
  *
  * ATTENTION: it is slightly different from Stream Request:
  * Read Data (Stream Response, this class) Length is memory bandwidth
  * Write Data (Stream Request) Length can be scratchpad bank width
  *
  * @param memNode   Memory Node
  * @param ivpsParam IVP Parameters
  * @param ovpsParam OVP Parameters
  * @param p         CDE
  */
class StreamResponse(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit val p: Parameters)
    extends Bundle {
  require(memNode.needStrResponse, s"Only DMA or SPM or GEN or REG need stream response")

  /* ---------- Derived Parameters ---------- */
  val nodeType: DSAGenNodeType = memNode.nodeType

  /* ---------- Hardware Fields ---------- */

  // Whether this entry is valid
  val valid: Bool = Bool()

  // Stream Request Meta Info
  val meta: StreamMeta = new StreamMeta(memNode, ivpsParam, ovpsParam, hasMStatus = false)

  // Stream State: The current state of stream
  val state: Option[StreamState] =
    if (memNode.streamStated) Some(new StreamState()) else None

  // Address Bits, I feel this is useless, why read response need addr, we have ROB id already
  val addr: Option[UInt] = None // if(memNode.needAddr) Some(UInt(memNode.addrBits.W)) else None

  // Mask
  val mask: Option[UInt] = if (memNode.needStrResponse) Some(UInt(memNode.bandwidth.W)) else None

  // Data Type Payload for Read Response, this is different from stream request, BE ATTENTION !!!
  val readData: Option[UInt] = if (memNode.needStrResponse) Some(UInt(memNode.bandBits.W)) else None

  // Group read data into memory unit
  def readDataGroup: Option[Vec[UInt]] =
    readData match {
      case Some(rData) => Some(VecInit(groupBitsAs(rData, memNode.memUnitBits)))
      case None        => None
    }
}
