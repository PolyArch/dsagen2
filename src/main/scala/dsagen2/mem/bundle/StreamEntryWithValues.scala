package dsagen2.mem.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.top.diplomacy.DSANodeType._

/** This is the stream entry selected from stream table, together with possible indirect values from OVP, and values
  * that will be written into memory
  *
  * @param memNode   Memory Node Parameters
  * @param ivpsParam IVP Parameters
  * @param ovpsParam OVP Parameters
  * @param p         CDE
  */
class StreamEntryWithValues(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit val p: Parameters)
    extends Bundle {

  /* ---------- Derived Parameters ----------*/
  def numIVP: Int = ivpsParam.length

  def numOVP: Int = ovpsParam.length

  /* ---------- Hardware Fields ---------- */

  // Selected Outstanding Stream Entry from Stream Table
  val currStrEntry: StreamEntryOutstanding = new StreamEntryOutstanding(memNode, ivpsParam, ovpsParam)

  // Possible Indirect INDEX Value from Output Vector Port
  // Scratchpad Memory can take more than one index value from ovp at a time
  // DMA: vector length = 1
  // SPM: vector length = #bank
  val indIdxValues: Option[Vec[UInt]] = memNode.nodeType match {
    case DirectMemoryAccess =>
      if (memNode.IndirectIndexStream) {
        require(
          memNode.maxIndIndexBits <= memNode.bandBits,
          s"The maximum index data type is ${memNode.maxIndIndexBits}, " +
            s"it is wider than memory bitwidth ${memNode.bandBits}"
        )
        // Indirect DMA only allows one request at a time
        Some(Vec(1, UInt(memNode.maxIndIndexBits.W)))
      } else None
    case ScratchpadMemory =>
      // Indirect SPM allows up to #bank request, so we provides index values as write Bundle
      if (memNode.IndirectIndexStream) Some(Vec(memNode.bandwidth, UInt(memNode.memUnitBits.W))) else None
    case RecurrenceEngine => None
    case DiscardEngine    => None
    case GenerateEngine   =>
      // Indirect Generation can only take one value at a time
      if (memNode.IndirectIndexStream) Some(Vec(1, UInt(memNode.maxIndIndexBits.W))) else None
    case RegisterEngine => None
    case errType: Any => require(requirement = false, s"Node Type $errType does not make sense"); None
  }
  val indIdxValids: Option[Vec[Bool]] = memNode.nodeType match {
    case DirectMemoryAccess => if (memNode.IndirectIndexStream) Some(Vec(1, Bool())) else None
    case ScratchpadMemory   => if (memNode.IndirectIndexStream) Some(Vec(memNode.bandwidth, Bool())) else None
    case RecurrenceEngine   => None
    case DiscardEngine      => None
    // Indirect Generation can only take one value at a time
    case GenerateEngine => if (memNode.IndirectIndexStream) Some(Vec(1, Bool())) else None
    case RegisterEngine => None
    case errType: Any => require(requirement = false, s"Node Type $errType does not make sense"); None
  }

  // Possible Indirect S2D Value from Output Vector Port, this should be ONE a time
  // At least one byte wide
  val indS2DValue: Option[UInt] = memNode.nodeType match {
    case DirectMemoryAccess => if (memNode.IndirectStride2DStream) Some(UInt(memNode.maxIndStride2DBits.W)) else None
    case ScratchpadMemory   => if (memNode.IndirectStride2DStream) Some(UInt(memNode.maxIndStride2DBits.W)) else None
    case RecurrenceEngine   => None
    case DiscardEngine      => None
    case GenerateEngine     => if (memNode.IndirectStride2DStream) Some(UInt(memNode.maxIndStride2DBits.W)) else None
    case RegisterEngine     => None
    case errType: Any => require(requirement = false, s"Node Type $errType does not make sense"); None
  }

  // Possible Indirect S2D Valid
  val indS2DValid: Option[Bool] = memNode.nodeType match {
    case DirectMemoryAccess => if (memNode.IndirectStride2DStream) Some(Bool()) else None
    case ScratchpadMemory   => if (memNode.IndirectStride2DStream) Some(Bool()) else None
    case RecurrenceEngine   => None
    case DiscardEngine      => None
    case GenerateEngine     => if (memNode.IndirectStride2DStream) Some(Bool()) else None
    case RegisterEngine     => None
    case errType: Any => require(requirement = false, s"Node Type $errType does not make sense"); None
  }

  // Possible Indirect L1D Value from Output Vector Port, this should be ONE a time
  // At least one byte wide
  val indL1DValue: Option[UInt] = memNode.nodeType match {
    case DirectMemoryAccess => if (memNode.IndirectLength1DStream) Some(UInt(memNode.maxIndLength1DBits.W)) else None
    case ScratchpadMemory   => if (memNode.IndirectLength1DStream) Some(UInt(memNode.maxIndLength1DBits.W)) else None
    case RecurrenceEngine   => None
    case DiscardEngine      => None
    case GenerateEngine     => if (memNode.IndirectLength1DStream) Some(UInt(memNode.maxIndLength1DBits.W)) else None
    case RegisterEngine     => None
    case errType: Any => require(requirement = false, s"Node Type $errType does not make sense"); None
  }

  // Possible Indirect L1D Valid
  val indL1DValid: Option[Bool] = memNode.nodeType match {
    case DirectMemoryAccess => if (memNode.IndirectLength1DStream) Some(Bool()) else None
    case ScratchpadMemory   => if (memNode.IndirectLength1DStream) Some(Bool()) else None
    case RecurrenceEngine   => None
    case DiscardEngine      => None
    case GenerateEngine     => if (memNode.IndirectLength1DStream) Some(Bool()) else None
    case RegisterEngine     => None
    case errType: Any => require(requirement = false, s"Node Type $errType does not make sense"); None
  }

  // Write Data, data bits from ovp that will be written into memory
  val writeVecData: Option[Vec[UInt]] =
    if (memNode.canDoWrite) Some(Vec(memNode.writeWidth, UInt(memNode.memUnitBits.W))) else None

  // Write Mask , indicate data valid
  // TODO: when sent to AGU, this is sent but not used, since AGU will generate mask again
  //    but still I want to keep it
  // The number of set bit in here should be equal to the number of set bit in AGU generated mask
  // difference is that AGU generated mask is distributed, but this one is aggregated at lower part
  val writeValids: Option[Vec[Bool]] =
    if (memNode.canDoWrite) Some(Vec(memNode.writeWidth, Bool())) else None

  // Get the Write data without predication
  def writeData: Vec[UInt] =
    writeVecData.getOrElse(WireInit(VecInit(Seq.fill(memNode.bandwidth)(0.U(memNode.memUnitBits.W)))))
}
