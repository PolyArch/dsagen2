package dsagen2.mem.bundle

import chisel3._
import chisel3.util.log2Ceil
import dsagen2.mem.config.MemNodeParameters
import dsagen2.top.config.DSAFixedConfig.{memOpRead, memOpWrite}
import freechips.rocketchip.rocket.MStatus

/** A even simplified version of stream request, only kept info that memory cares, no stream pattern
  * TODO: I don't which part is different from Memory Response, but let keep it different class
  *
  * @param memNode Memory Node Parameters
  */
class MemRequest(val memNode: MemNodeParameters) extends Bundle {
  /*---------- Hardware Fields ----------*/

  // valid
  val valid: Bool = Bool()

  // Virtual Address
  val vaddr: UInt = UInt(memNode.addrBits.W)

  // Operation
  val memOp: UInt = UInt(memNode.memOperationBits.W)

  // MStatus needed by DMA
  val mStatus: Option[MStatus] = if (memNode.isDMA) Some(new MStatus) else None

  // Memory Data Type Exp, for scratchpad atomic operation
  val memDataTypeExp: Option[UInt] =
    if (memNode.numMemDataTypeExp > 1) Some(UInt(memNode.memDataTypeBits.W)) else None

  // Mask
  val mask: UInt =
    if (memNode.isDMA) {
      UInt(memNode.bandwidth.W)
    } else if (memNode.isSPM) {
      UInt(memNode.spmBankWidth.W)
    } else {
      require(requirement = false, s"I don't other module need mask")
      UInt(1.W)
    }

  // Data
  val data: UInt =
    if (memNode.isDMA) {
      UInt(memNode.bandBits.W)
    } else if (memNode.isSPM) {
      UInt(memNode.spmBankBitWidth.W)
    } else {
      require(requirement = false, s"I don't other module need response data")
      UInt(1.W)
    }

  // Reorder Buffer ID
  val robId: Option[UInt] = if (memNode.needROB) Some(UInt(log2Ceil(memNode.numPendingRequest).W)) else None

  // Request Index (position in the vector request port, for read response routing)
  val reqIdx: Option[UInt] =
    if (memNode.numMemReqPort > 1)
      Some(UInt(log2Ceil(memNode.numMemReqPort).W))
    else None

  /* Utility */

  // Check whether current request is atomic operation, neither read nor write
  def isAtomOp: Bool = valid && memOp =/= memOpRead && memOp =/= memOpWrite

  def isRead: Bool = valid && memOp === memOpRead

  def isWrite: Bool = valid && memOp === memOpWrite
}
