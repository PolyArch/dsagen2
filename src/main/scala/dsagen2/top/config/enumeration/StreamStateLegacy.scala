package dsagen2.top.config.enumeration

import chisel3.{Bool, UInt, VecInit, Wire}
import dsagen2.top.bundle.{CanBeEncoded, EnumBundle}

import scala.collection.Set

object StreamStateLegacy extends Enumeration with CanBeEncoded {
  type StreamState = Value
  // Since multiple state can be valid at the same time, it should be mask encoded
  override val encMethod: EnumEncodeMethod.Value = EnumEncodeMethod.maskEncode
  // For each bundle, it can only support subnet of StreamState, so it should be trimmable
  override val trimmable: Boolean = true
  // It is not one hot
  override val isOneHot: Boolean = false

  // Define how two stream state EnumBundle are merged together
  override def merge(source0: EnumBundle, source1: EnumBundle): EnumBundle = {
    // Check the Enumeration Type
    def requireIsStreamState(bundle: EnumBundle): Unit = {
      require(bundle.enumGroup.getOrElse(this) == this, s"Bundle $bundle is not $this EnumBundle")
    }

    requireIsStreamState(source0);
    requireIsStreamState(source1)
    // Create merged Wire
    val mergeWire: EnumBundle = Wire(
      new EnumBundle(
        source0.enumerations ++ source1.enumerations, // enumeration should be the union set
        EnumEncodeMethod.maskEncode, // merged stream state EnumBundle is still maskEncoded
        supportInvalid = false, // since the enumeration set is union, I did not see why we need invalid
        isOneHot = false // stream state cannot be one hot
      )
    )
    // Build merged bits
    val mergeBits: Option[UInt] = mergeWire.enumSeq match {
      case Nil => None
      case mEnumSeq: Seq[Enumeration#Value] =>
        val mergeSeqBool: Seq[Bool] = mEnumSeq.map { mEnum =>
          source0.is(mEnum) || source1.is(mEnum) // the merge of the stream state is OR
        }
        Some(VecInit(mergeSeqBool).asUInt())
    }
    // Connect the merged Bits to merged Wire
    (mergeWire.enumHardwareField, mergeBits) match {
      case (Some(wF), Some(mF)) =>
        require(wF.getWidth == mF.getWidth)
        wF := mF
      case (None, None) =>
      case _            => require(false)
    }
    // Return EnumBundle
    mergeWire
  }

  ////////////////////////////////////////
  ///////   Stream Specification   ///////
  ////////////////////////////////////////

  // LSB
  val StreamStart: StreamState = Value("StreamStart")
  val StreamEnd:   StreamState = Value("StreamEnd")
  // 1D Stream Start and End
  val Stream1DStart: StreamState = Value("Stream1DStart")
  val Stream1DEnd:   StreamState = Value("Steam1DEnd")
  // 2D Stream Start and End
  val Stream2DStart: StreamState = Value("Stream2DStart")
  val Stream2DEnd:   StreamState = Value("Stream2DEnd")
  // MSB

  // Return the full set of Stream State Enumeration
  def streamStateSet: Set[Enumeration#Value] =
    Set(StreamStart, StreamEnd, Stream1DStart, Stream1DEnd, Stream2DStart, Stream2DEnd)

  // Filter out the Stream State Enumeration from a set of Enumeration
  def getStreamState(potentialSet: Set[Enumeration#Value]): Set[Enumeration#Value] =
    potentialSet.filter { enum => enum.isInstanceOf[StreamState] }
}
