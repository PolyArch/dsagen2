package dsagen2.mem.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.log2Ceil
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.top.config.DSAFixedConfig.LINEAR_PADDING_BITS
import dsagen2.top.diplomacy.DSANodeType._
import freechips.rocketchip.rocket.MStatus

/** Stream Meta Info when request and response (Short Version of Stream Entry)
  *
  * @param memNode   Memory Node Parameter
  * @param ivpsParam IVP Parameters
  * @param ovpsParam OVP Parameters
  * @param p         CDE
  */
class StreamMeta(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter],
  val hasMStatus: Boolean = true
)(
  implicit val p: Parameters)
    extends Bundle {

  /* ---------- Derived Parameters ---------- */
  val nodeType: DSAGenNodeType = memNode.nodeType

  def numIVP: Int = ivpsParam.length

  /* ---------- Hardware Fields ---------- */

  // MStatus for DMA
  val mStatus: Option[MStatus] = if (memNode.isDMA && hasMStatus) Some(new MStatus) else None

  // The target local port ID, for read only at this stage since write and atomic operation dont care
  val targetLocalPortId: Option[UInt] = if (numIVP > 1) Some(UInt(log2Ceil(numIVP).W)) else None

  // Data Type of element of this request
  val memDataTypeExp: Option[UInt] =
    if (nodeType != RegisterEngine && memNode.memDataTypeBits > 0) Some(UInt(memNode.memDataTypeBits.W))
    else None

  // Data Type for stream generation
  val constDataTypeExp: Option[UInt] =
    if (nodeType == GenerateEngine && memNode.constDataTypeBits > 0) Some(UInt(memNode.constDataTypeBits.W))
    else None

  // Memory Operation
  // DMA and SPM: up to 3-bit field, read, write and 6 kinds of atomic operation
  // REC: None, just recurrence
  // DIS and GEN: None, either discard or generate
  // REG: 1-bit field, read (cpu2ivp) and write (ovp2cpu)
  val memOperation: Option[UInt] =
    if (memNode.memOperationBits > 0)
      Some(UInt(memNode.memOperationBits.W))
    else None

  // Stream Pattern Type: Linear or Indirect
  // TODO: this field seems not used, please remove it in the future
  // DMA and SPM: optional 1-bit, can support both
  // REC: None, only linear
  // DIS: None, only linear
  // REG: None, scalar, for cpu2ivp, only linear
  val LinOrInd: Option[Bool] = {
    if (
      memNode.supportLinear && memNode.supportIndirect &&
      (nodeType == DirectMemoryAccess || nodeType == ScratchpadMemory)
    ) Some(Bool())
    else None
  }

  // Linear Padding Mode (Pass to Input Vector Port for Linear Padding)
  val linearPadding: Option[UInt] =
    if (memNode.LinearPadding) Some(UInt(LINEAR_PADDING_BITS.W)) else None

  // isDescending, this will change the request ID in vector
  val isDescend: Option[Bool] = if (memNode.supportDescend) Some(Bool()) else None
}
