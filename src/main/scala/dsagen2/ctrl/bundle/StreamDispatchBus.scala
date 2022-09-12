package dsagen2.ctrl.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.log2Ceil
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.module.DSAGen
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.tile.XLen

/** The Bus Bundle from Stream Dispatcher to each of the memory node. The output direction is from Dispatcher to Memory node
  */
class StreamDispatchBus(val dsagen: DSAGen)(implicit val p: Parameters) extends Bundle {

  /* ---------- Hardware Fields ---------- */

  // Whether there is new stream entry
  val valid: Bool = Bool()

  // The MStatus used for DMA Access, mstatus is only for DMA access, please remove it if there is no DMA node
  val mStatus: Option[MStatus] = if (dsagen.numDMA > 0) Some(new MStatus) else None

  // The local port ID, input vector port and output vector port can have same local port ID
  // DMA: IVP / OVP based on memory operation
  // SPM: IVP / OVP based on memory operation
  // REC: OVP
  // DIS: OVP
  // GEN: IVP / OVP based on generation type
  // REG: OVP
  val targetLocalPortId: Option[UInt] =
    if (dsagen.maxNumVP > 1) Some(UInt(log2Ceil(dsagen.maxNumVP).W)) else None

  // The data type of stream
  val memDataTypeExp: Option[UInt] =
    if (dsagen.maxMemDataTypeBits > 0) Some(UInt(dsagen.maxMemDataTypeBits.W)) else None

  // The data type of constant stream, dedicated to generate engine
  val constDataTypeExp: Option[UInt] =
    if (dsagen.maxConstDataTypeBits > 0) Some(UInt(dsagen.maxConstDataTypeBits.W)) else None

  // Type of the memory,
  val memType: UInt = UInt(3.W)

  // The memory operation type, read:0, write:1, atomic operation : 2 - 7
  val memOperation: UInt = UInt(dsagen.memOpBits.W)

  // Whether this stream entry is linear or indirect stream
  // false: Linear; true: Indirect
  val LinOrInd: Option[Bool] = if (dsagen.supportLinearAndIndirect) Some(Bool()) else None

  // Start Point, for DMA and SPM it is the start address;
  //              for REC, DIS, it is useless;
  //              for GEN, it is the start point of numerical sequence;
  //              for REG, it is the register value to be sent to input vector port
  val startPoint: UInt = UInt(p(XLen).W)

  // Stride 1D, indicate the distance between inner most loop access
  val stride1D: Option[UInt] =
    if (dsagen.maxStride1DBits > 0) Some(UInt(dsagen.maxStride1DBits.W)) else None

  // Number of linear dimension; 0: 1D, 1: 2D, 2: 3D, 3: 4D (not supported)
  val numLinDim: Option[UInt] =
    if (dsagen.maxNumLinearDim >= 2) Some(UInt(log2Ceil(dsagen.maxNumLinearDim).W))
    else None

  // Linear Padding Mode
  val linearPadding: Option[UInt] =
    if (dsagen.supportLinearPadding) Some(UInt(LINEAR_PADDING_BITS.W)) else None

  // The recurrence input vector port, dedicated to recurrence engine
  val recIVPortId: Option[UInt] =
    if (dsagen.numREC == 0 || dsagen.numIVP <= 1) None // if there is no recurrence engine, or #theOtherVP is 1
    else Some(UInt(log2Ceil(dsagen.numIVP).W))

  // Length 1D (remaining repeated time of doing linear 1D stream, 0 means length 1d = 1)
  val initLength1D: Option[UInt] =
    if (dsagen.maxLength1DBits > 0) Some(UInt(dsagen.maxLength1DBits.W))
    else None

  // Stride 2D (delta to the start point per linear 1D stream, signed)
  val stride2D: Option[UInt] =
    if (dsagen.maxStride2DBits > 0) Some(UInt(dsagen.maxStride2DBits.W)) else None

  // Stretch 2D (delta to the length 1D per linear 1D stream, signed)
  val stretch2D: Option[UInt] =
    if (dsagen.maxStretch2DBits > 0) Some(UInt(dsagen.maxStretch2DBits.W)) else None

  // Length 2D (remaining repeated time of doing linear 2D stream, 0 means length 2d = 1)
  val initLength2D: Option[UInt] =
    if (dsagen.maxLength2DBits > 0) Some(UInt(dsagen.maxLength2DBits.W))
    else None

  // Stretch 3D to 2D (delta to length 2D per linear 2D stream, signed)
  val stretch3D2D: Option[UInt] =
    if (dsagen.maxStretch3D2DBits > 0) Some(UInt(dsagen.maxStretch3D2DBits.W)) else None

  // Stretch 3D to 1D (delta to length 1D per linear 2D stream, signed)
  val stretch3D1D: Option[UInt] =
    if (dsagen.maxStretch3D1DBits > 0) Some(UInt(dsagen.maxStretch3D1DBits.W)) else None

  // Delta to Stride 2D (delta to stride 2D per linear 2D stream, signed)
  val deltaStride2D: Option[UInt] =
    if (dsagen.maxDeltaStride2DBits > 0) Some(UInt(dsagen.maxDeltaStride2DBits.W)) else None

  // Delta to Stretch 2D (delta to stretch 2D per linear 2D stream, signed)
  val deltaStretch2D: Option[UInt] =
    if (dsagen.maxDeltaStretch2DBits > 0) Some(UInt(dsagen.maxDeltaStretch2DBits.W)) else None

  // Stride 3D (delta to the start point per linear 2D stream, signed)
  val stride3D: Option[UInt] =
    if (dsagen.maxStride3DBits > 0) Some(UInt(dsagen.maxStride3DBits.W)) else None

  // Length 3D (remaining repeated time of doing linear 3D stream, 0 means length 3d = 1)
  val initLength3D: Option[UInt] =
    if (dsagen.maxLength3DBits > 0) Some(UInt(dsagen.maxLength3DBits.W))
    else None

  // Whether or not support index stream from output vector port: 0: zero stream; 1: indirect from ovp
  val indirectIdxStream: Option[Bool] = if (dsagen.supportIndirectIdx) Some(Bool()) else None

  // Whether or not support dual mode for Stride 2D stream: 0:linear; 1: indirect from ovp
  val indirectS2DStream: Option[Bool] = if (dsagen.supportDualModeStride2D) Some(Bool()) else None

  // Whether or not support dual mode for Length 1D stream: 0:linear; 1: indirect from ovp
  val indirectL1DStream: Option[Bool] = if (dsagen.supportDualModeLength1D) Some(Bool()) else None

  // The indirect dimension, 0: indirect 1D; 1: indirect 2D; 2: indirect 3D (not supported for now)
  val numIndirectDim: Option[UInt] =
    if (dsagen.maxNumIndirectDim >= 2) Some(UInt(log2Ceil(dsagen.maxNumIndirectDim).W))
    else None

  // If the index stream from port is supported, ovp port id should be specified
  val indexOVPortId: Option[UInt] =
    if (dsagen.supportIndirectIdx && dsagen.numOVP > 1) Some(UInt(log2Ceil(dsagen.numOVP).W))
    else None

  // If the indirect stride 2D from port is supported, ovp port should be specified
  val stride2DOVPortId: Option[UInt] =
    if (dsagen.supportIndirectS2D && dsagen.numOVP > 1) Some(UInt(log2Ceil(dsagen.numOVP).W))
    else None

  // If the indirect length 1D from port is supported, ovp port should be specified
  val length1DOVPortId: Option[UInt] =
    if (dsagen.supportIndirectL1D && dsagen.numOVP > 1) Some(UInt(log2Ceil(dsagen.numOVP).W))
    else None

  // If index stream is supported, we should specify the data type of it
  val idxStrDataType: Option[UInt] =
    if (dsagen.maxIdxStrDataTypeBits > 0 && dsagen.supportIndirectIdx)
      Some(UInt(dsagen.maxIdxStrDataTypeBits.W))
    else None

  // If stride 2D stream is supported, we should specify the data type of it
  val s2dStrDataType: Option[UInt] =
    if (dsagen.maxStride2DStrDataTypeBits > 0 && dsagen.supportIndirectS2D)
      Some(UInt(dsagen.maxStride2DStrDataTypeBits.W))
    else None

  // If length 1D stream is supported
  val l1dStrDataType: Option[UInt] =
    if (dsagen.maxLength1DStrDataTypeBits > 0 && dsagen.supportIndirectL1D)
      Some(UInt(dsagen.maxLength1DStrDataTypeBits.W))
    else None

  // Destination Register for Register Engine
  val rd: Option[UInt] =
    if (dsagen.numREG > 0) Some(UInt(5.W)) else None

  /* ---------- Utility ---------- */

  // Get the input vector port id for recurrence engine
  //  def recIVPortId: UInt =
  //    if (dsagen.moreIVP) targetLocalPortId.getOrElse(0.U) // if #IVP >= #OVP, means targetLocalPortId is ivp port id
  //    else recLocalPortId.getOrElse(0.U) // if #IVP < #OVP, means recLocalPortId is ivp port id

  // Get the output vector port id for recurrence engine
  def recOVPortId: UInt = targetLocalPortId.getOrElse(0.U)

  // Get the memory type
  def isREC: Bool = Wire(memType) === recMemType

  // Check the memory operation type
  def isRead: Bool = memOperation === memOpRead

  // Check whether the stream is indirect stream
  def isIndirect: Bool =
    (dsagen.supportIndirect, LinOrInd) match {
      case (true, Some(pattern)) => pattern
      case (false, _)            => false.B
      case (true, None)          => true.B
    }

  // Check whether the stream is linear stream
  def isLinear: Bool =
    (dsagen.supportLinear, LinOrInd) match {
      case (true, Some(pattern)) => !pattern
      case (true, None)          => true.B
      case (false, _)            => false.B
    }

  /*  // Illegal memory type
    def memTypeIllegal : Bool = Wire(memType) >= NUM_MEM_TYPE.U

    // Illegal for Recurrence Stream: There is no indirect recurrence stream
    def recIllegal : Bool = isREC && Wire(isIndirect)

    // Illegal for Generate Stream: There is no indirect recurrence stream
    def genIllegal : Bool = Wire(memType=== genMemType)  && Wire(isIndirect)

    // Illegal for Discard Stream: There is no indirect discard stream
    def disIllegal : Bool = isDIS && Wire(isIndirect)

    // Stream Illegal
    def strIllegal : Bool = recIllegal || memTypeIllegal || genIllegal || disIllegal*/

  // Hardware Sanity Check TODO: Hardware check
  //assert(!strIllegal, s"This stream entry is illegal")
}
