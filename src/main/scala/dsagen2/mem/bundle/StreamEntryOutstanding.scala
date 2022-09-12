package dsagen2.mem.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}

// This is the outstanding stream entry in the stream table
class StreamEntryOutstanding(
  val memNode:      MemNodeParameters,
  val ivpsParam:    Seq[Mem2IVPParameter],
  val ovpsParam:    Seq[OVP2MemParameter],
  val initRemove:   Boolean = false,
  val portIDRemove: Boolean = false
)(
  implicit val p: Parameters)
    extends Bundle {
  /* ---------- Extract Parameters ---------- */

  /* ---------- Hardware Fields ---------- */

  // Original Stream Entry from Dispatcher
  val origStrEntry: StreamEntry = new StreamEntry(memNode, ivpsParam, ovpsParam, initRemove)

  // Outstanding Start Point in linear 1D stream
  val currStartPoint1D: Option[UInt] = origStrEntry.startPoint match {
    case Some(oriStartPoint) =>
      if (memNode.Linear1DEnabled && memNode.MaxAbsStride1D > 0) Some(UInt(oriStartPoint.getWidth.W)) else None
    case None => None
  }

  // Outstanding Start Point in linear 2D stream
  val currStartPoint2D: Option[UInt] = origStrEntry.startPoint match {
    case Some(oriStartPoint) =>
      if (memNode.IndirectStride2DStream || memNode.Linear2DEnabled && memNode.MaxAbsStride2D > 0)
        Some(UInt(oriStartPoint.getWidth.W))
      else None
    case None => None
  }

  // Outstanding Start Point in linear 3D stream
  val currStartPoint3D: Option[UInt] = origStrEntry.startPoint match {
    case Some(oriStartPoint) =>
      if (memNode.Linear3DEnabled && memNode.MaxAbsStride3D > 0) Some(UInt(oriStartPoint.getWidth.W)) else None
    case None => None
  }

  // Outstanding Length 1D in linear 1D stream
  val currLength1Din1D: Option[UInt] = origStrEntry.initLength1D match {
    case Some(initL1D) => Some(UInt(initL1D.getWidth.W))
    case None          => None
  }

  // Outstanding Length 1D in linear 2D stream
  val currLength1Din2D: Option[UInt] = origStrEntry.initLength1D match {
    case Some(initL1D) =>
      if (memNode.IndirectLength1DStream || memNode.Linear2DEnabled && memNode.MaxAbsStretch2D > 0)
        Some(UInt(initL1D.getWidth.W))
      else None
    case None => None
  }

  // Outstanding Length 1D in linear 3D stream
  val currLength1Din3D: Option[UInt] = origStrEntry.initLength1D match {
    case Some(initL1D) =>
      if (memNode.Linear3DEnabled && memNode.MaxAbsStretch3D1D > 0) Some(UInt(initL1D.getWidth.W)) else None
    case None => None
  }

  // Outstanding Length 2D in linear 2D stream
  val currLength2Din2D: Option[UInt] = origStrEntry.initLength2D match {
    case Some(initL2D) => Some(UInt(initL2D.getWidth.W))
    case None          => None
  }

  // Outstanding Length 2D in linear 3D stream
  val currLength2Din3D: Option[UInt] = origStrEntry.initLength2D match {
    case Some(initL2D) =>
      if (memNode.Linear3DEnabled && memNode.MaxAbsStretch3D2D > 0) Some(UInt(initL2D.getWidth.W)) else None
    case None => None
  }

  // Outstanding Length 3D in linear 3D stream
  val currLength3D: Option[UInt] = origStrEntry.initLength3D match {
    case Some(initL3D) => Some(UInt(initL3D.getWidth.W))
    case None          => None
  }

  // Outstanding Stride 2D in linear 3D stream
  val currStride2D: Option[UInt] = origStrEntry.stride2D match {
    case Some(stride2D) =>
      if (memNode.Linear3DEnabled && memNode.MaxAbsDeltaStride2D > 0)
        Some(UInt(stride2D.getWidth.W))
      else None
    case None => None
  }

  // Outstanding Stretch 2D in linear 3D stream
  val currStretch2D: Option[UInt] = origStrEntry.stretch2D match {
    case Some(stretch2D) =>
      if (memNode.Linear3DEnabled && memNode.MaxAbsDeltaStretch2D > 0)
        Some(UInt(stretch2D.getWidth.W))
      else None
    case None => None
  }

  // Start of Stream
  val StartOfStream: Bool = Bool()

  // Start of Stream 1D
  val StartOf1D: Bool = Bool()

  // Start of Stream 2D
  val StartOf2D: Bool = Bool()
}
