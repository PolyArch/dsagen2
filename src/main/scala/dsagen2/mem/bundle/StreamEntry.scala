package dsagen2.mem.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.diplomacy.DSANodeType._
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.tile.{CoreBundle, XLen}

/** This is the stream entry depends on each memory node type, it will be similar with the StreamDispatchBus, which can be
  * viewed as a super set of this bundle
  *
  * @param memNode      The parameter of memory node
  * @param ivpsParam    IVP Parameters
  * @param ovpsParam    OVP Parameters
  * @param initRemove   Remove init Length1D/Length2D if it is not needed, why this remove is optional? because we
  *                     want to keep it during stream dispatch but it can be optional removed in stream table
  * @param portIDRemove Remove for saving bit in stream table
  * @param p            CDE Parameters
  */
class StreamEntry(
  val memNode:      MemNodeParameters,
  val ivpsParam:    Seq[Mem2IVPParameter],
  val ovpsParam:    Seq[OVP2MemParameter],
  val initRemove:   Boolean = false,
  val portIDRemove: Boolean = false
)(
  implicit p: Parameters)
    extends CoreBundle {
  /* ---------- Derived Parameters ---------- */
  val XLEN: Int = p(XLen)

  def numIVP: Int = ivpsParam.length

  def numOVP: Int = ovpsParam.length

  def maxNumVP: Int = numIVP.max(numOVP)

  val nodeType: DSAGenNodeType = memNode.nodeType

  /* ---------- Sanity Check ---------- */

  // Memory Node must have at least one input vector port, except Discard Node
  if (nodeType != DiscardEngine && nodeType != RegisterEngine) {
    require(numIVP > 0, s"Memory Node $nodeType has only $numIVP input vector port")
  }

  // Memory Node must have at least one output vector port, except Generate Node
  if (nodeType != GenerateEngine) {
    require(numOVP > 0, s"Memory Node $nodeType has only $numOVP output vector port")
  }

  /* ---------- Hardware Fields ----------*/

  // Whether this entry is valid
  val valid: Bool = Bool()

  // The MStatus used for DMA Access
  val mStatus: Option[MStatus] = if (memNode.isDMA) Some(new MStatus) else None

  // The target local port ID
  val targetLocalPortId: Option[UInt] = if (maxNumVP > 1 && !portIDRemove) Some(UInt(log2Ceil(maxNumVP).W)) else None

  // All memory node needs the data type of stream, except Register Engine, which always be XLEN
  val memDataTypeExp: Option[UInt] =
    if (nodeType != RegisterEngine && memNode.memDataTypeBits > 0) Some(UInt(memNode.memDataTypeBits.W))
    else None

  // Generate Engine Dedicated, The Data Type of generated stream
  val constDataTypeExp: Option[UInt] =
    if (nodeType == GenerateEngine && memNode.constDataTypeBits > 0) Some(UInt(memNode.constDataTypeBits.W))
    else None

  // Memory Operation
  // DMA and SPM: up to 3-bit field, at least 1-bit: read, write and optional 6 kinds of atomic operation
  // REC: None, just recurrence
  // DIS and GEN: None, either discard or generate
  // REG: 1-bit field, read (cpu2ivp) and write (ovp2cpu)
  val memOperation: Option[UInt] = if (memNode.memOperationBits > 0) Some(UInt(memNode.memOperationBits.W)) else None

  // Stream Pattern Type: Linear or Indirect, 0: linear, 1: indirect
  // DMA and SPM: optional 1-bit, can support both
  // REC: None, only linear
  // DIS and GEN: None, only linear
  // REG: None, scalar
  val LinOrInd: Option[Bool] = {
    if (
      memNode.supportLinear && memNode.supportIndirect &&
      (nodeType == DirectMemoryAccess || nodeType == ScratchpadMemory)
    ) Some(Bool())
    else None
  }

  // Start Point
  // DMA and SPM: Start Address of Stream
  // REC, DIS: None
  // GEN: Start Point of Generated Numerical Sequence
  // REG: Value going to be sent to IVP (read, cpu2ivp)
  val startPoint: Option[UInt] = nodeType match {
    case DirectMemoryAccess =>
      // println(s"coreMaxAddrBits = $coreMaxAddrBits") 40 since last time
      Some(UInt(coreMaxAddrBits.W))
    case ScratchpadMemory => Some(UInt(memNode.addrBits.W))
    case RecurrenceEngine => None
    case DiscardEngine    => None
    case GenerateEngine   => Some(UInt(XLEN.W))
    case RegisterEngine   => Some(UInt(XLEN.W))
    case errType: Any =>
      require(requirement = false, s"Node $errType is not supported")
      None
  }

  // Stride 1D
  // DMA, SPM, GEN: Optional based on MaxAbsStride1D
  // DIS, REC, REG: None
  val stride1D: Option[UInt] = nodeType match {
    case DirectMemoryAccess => if (memNode.stride1DBits > 0) Some(UInt(memNode.stride1DBits.W)) else None
    case ScratchpadMemory   => if (memNode.stride1DBits > 0) Some(UInt(memNode.stride1DBits.W)) else None
    case RecurrenceEngine   => Some(UInt(1.W)) // Recurrence Engine must has stride1D == 1.U(1.W), ascend only
    case DiscardEngine      => Some(UInt(1.W)) // Discard Engine must has stride1D == 1.U(1.W), ascend only
    case GenerateEngine     => if (memNode.stride1DBits > 0) Some(UInt(memNode.stride1DBits.W)) else None
    case RegisterEngine     => None
    case errType: Any =>
      require(requirement = false, s"Node $errType is not supported for stride 1D")
      None
  }

  // Number of Linear Dimension
  // DMA, SPM, GEN: Optional based on number of linear dimension
  // REC, DIS: None, since it is always linear 1D
  // REG: None, scalar
  val numLinDim: Option[UInt] = nodeType match {
    case DirectMemoryAccess =>
      if (memNode.numLinearDimension > 1) Some(UInt(log2Ceil(memNode.numLinearDimension).W)) else None
    case ScratchpadMemory =>
      if (memNode.numLinearDimension > 1) Some(UInt(log2Ceil(memNode.numLinearDimension).W)) else None
    case RecurrenceEngine => None
    case DiscardEngine    => None
    case GenerateEngine =>
      if (memNode.numLinearDimension > 1) Some(UInt(log2Ceil(memNode.numLinearDimension).W)) else None
    case RegisterEngine => None
    case errType: Any =>
      require(requirement = false, s"Node $errType is not supported for number of linear dimension")
      None
  }

  // Linear Padding Mode
  val linearPadding: Option[UInt] =
    if (memNode.LinearPadding) Some(UInt(LINEAR_PADDING_BITS.W)) else None

  // Recurrence Engine Dedicated, the local port id of the side whose amount is less
  val recIVPortId: Option[UInt] =
    if (nodeType == RecurrenceEngine && numIVP > 1) Some(UInt(log2Ceil(numIVP).W)) else None

  // Initial Length 1D, all memory nodes can support it, but not necessary
  // If initial removed, init length 1D is only needed when there is higher dimension pattern
  val initLength1D: Option[UInt] =
    if (memNode.length1DBits > 0) {
      if (initRemove) if (memNode.numLinearDimension > 1) Some(UInt(memNode.length1DBits.W)) else None
      else Some(UInt(memNode.length1DBits.W))
    } else None

  // Stride 2D
  val stride2D: Option[UInt] =
    if (memNode.stride2DBits > 0) Some(UInt(memNode.stride2DBits.W)) else None

  // Stretch 2D
  val stretch2D: Option[UInt] =
    if (memNode.stretch2DBits > 0) Some(UInt(memNode.stretch2DBits.W)) else None

  // Initial Length 2D, can be removed if there is no higher dimension, usually removed in stream table
  val initLength2D: Option[UInt] =
    if (memNode.length2DBits > 0) {
      if (initRemove) if (memNode.numLinearDimension > 2) Some(UInt(memNode.length2DBits.W)) else None
      else Some(UInt(memNode.length2DBits.W))
    } else None

  // Stretch 3D to 2D
  val stretch3D2D: Option[UInt] =
    if (memNode.stretch3D2DBits > 0) Some(UInt(memNode.stretch3D2DBits.W)) else None

  // Stretch 3D to 1D
  val stretch3D1D: Option[UInt] =
    if (memNode.stretch3D1DBits > 0) Some(UInt(memNode.stretch3D1DBits.W)) else None

  // Delta to Stride 2D
  val deltaStride2D: Option[UInt] =
    if (memNode.deltaStride2DBits > 0) Some(UInt(memNode.deltaStride2DBits.W)) else None

  // Delta to Stretch 2D
  val deltaStretch2D: Option[UInt] =
    if (memNode.deltaStretch2DBits > 0) Some(UInt(memNode.deltaStretch2DBits.W)) else None

  // Stride 3D
  val stride3D: Option[UInt] =
    if (memNode.stride3DBits > 0) Some(UInt(memNode.stride3DBits.W)) else None

  // Initial Length 3D
  val initLength3D: Option[UInt] =
    if (memNode.length3DBits > 0) {
      if (initRemove) if (memNode.numLinearDimension > 3) Some(UInt(memNode.length3DBits.W)) else None
      else Some(UInt(memNode.length3DBits.W))
    } else None

  // Indirect Mode for index stream
  val indirectIdxStream: Option[Bool] = if (memNode.IndirectIndexStream) Some(Bool()) else None

  // Dual Mode for possible indirect Stride 2D stream
  val indirectS2DStream: Option[Bool] = if (memNode.dualModeStride2D) Some(Bool()) else None

  // Dual Mode for possible indirect Length 1D stream
  val indirectL1DStream: Option[Bool] = if (memNode.dualModeLength1D) Some(Bool()) else None

  // Output Vector Port that provides indirect index stream
  val indexOVPortId: Option[UInt] =
    if (memNode.IndirectIndexStream && numOVP > 1) Some(UInt(log2Ceil(numOVP).W)) else None

  // Output Vector Port that provides indirect stride 2D stream
  val stride2DOVPortId: Option[UInt] =
    if (memNode.IndirectStride2DStream && numOVP > 1) Some(UInt(log2Ceil(numOVP).W)) else None

  // Output Vector Port that provides indirect length 1D stream
  val length1DOVPortId: Option[UInt] =
    if (memNode.IndirectLength1DStream && numOVP > 1) Some(UInt(log2Ceil(numOVP).W)) else None

  // Indirect Index Stream Data Type
  val idxStrDataType: Option[UInt] =
    if (memNode.idxStrDataTypeBits > 0) Some(UInt(memNode.idxStrDataTypeBits.W)) else None

  // Indirect Stride 2D Stream Data Type
  val s2dStrDataType: Option[UInt] =
    if (memNode.s2dStrDataTypeBits > 0) Some(UInt(memNode.s2dStrDataTypeBits.W)) else None

  // Indirect Length 1D Stream Data Type
  val l1dStrDataType: Option[UInt] =
    if (memNode.l1dStrDataTypeBits > 0) Some(UInt(memNode.l1dStrDataTypeBits.W)) else None

  // Destination Register For Register Engine
  val rd: Option[UInt] = if (memNode.nodeType == RegisterEngine) Some(UInt(5.W)) else None

  /* ---------- Utility ----------*/

  // Check whether start point is misaligned
  def memMisaligned: Bool = {
    // Take out lowest bits based on exp
    val expIllegals: Seq[Bool] = for (exp <- 0 until memNode.numMemDataTypeExp) yield {
      if (exp == 0) {
        false.B
      } else {
        Wire(memDataTypeExp.get) === exp.U &&
        Wire(startPoint.getOrElse(0.U(8.W))).apply(exp - 1, 0).orR()
      }
    }
    Wire(valid) && VecInit(expIllegals).asUInt().orR()
  }

  // TODO: Hardware check
  //assert(!memMisaligned, s"Memory misaligned")
}
