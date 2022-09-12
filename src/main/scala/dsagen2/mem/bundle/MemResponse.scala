package dsagen2.mem.bundle

import chisel3._
import chisel3.util._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.top.config.DSAFixedConfig.memOpRead

/** A even simplified version of stream request, only kept info that memory cares, no stream pattern
  *
  * @param memNode Memory Node Parameters
  */
class MemResponse(val memNode: MemNodeParameters) extends Bundle {
  /*---------- Hardware Fields ----------*/

  // valid
  val valid: Bool = Bool()

  // Address, TODO: I feel it is useless, why we need to read address?
  val addr: UInt = UInt(memNode.addrBits.W)

  // Operation
  val memOp: UInt = UInt(memNode.memOperationBits.W)

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

  def isValidReadResp: Bool = valid && memOp === memOpRead
}
