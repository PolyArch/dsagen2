package dsagen2.misc.module

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.internal.requireIsChiselType
import chisel3.util._
import dsagen2.misc.bundle.MaskArrangementIO

class MaskAggregator[T <: Data](val gen: T, val numElem: Int) extends MultiIOModule {
  require(numElem > 0)
  // Get the chisel type of hardware
  val genType: T = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }
  // IO
  val io:         MaskArrangementIO[T] = IO(new MaskArrangementIO(genType, numElem))
  val sourceData: Vec[T] = VecInit(io.sources.map(_.bits))
  val sourceMask: Vec[Bool] = VecInit(io.sources.map(_.valid))
  // Edge Case Judge
  if (numElem == 1) {
    io.sinks.head <> io.sources.head
  } else if (numElem > 1) {

    // Combination Logics

    // Calculate the sink Mask
    val numActive: UInt = PopCount(sourceMask)
    io.sinks.map(_.valid).zipWithIndex.foreach { case (valid, idx) =>
      valid := idx.U < numActive
    }

    // Calculate the destination for each source data based on the number of previous set mask
    val destIdx: Seq[UInt] = for (idx <- 0 until numElem) yield {
      // Create the result wire
      val destWire: UInt = WireInit(0.U(log2Ceil(numElem).W))
      if (idx > 0) {
        // Get the previous mask
        val preMask:   Seq[Bool] = sourceMask.slice(0, idx)
        val numPreSet: UInt = PopCount(preMask)
        destWire := numPreSet
      }
      // Return
      destWire
    }

    // For each sink, use its index to match destIdx and valid to get OneHot, and then calculate the sourceIdx
    for (idx <- 0 until numElem) {
      // Destination match
      val destIdxMatch: Seq[Bool] = destIdx.map(_ === idx.U)
      // Combined with source valid
      val oneHot: Seq[Bool] = destIdxMatch.zip(sourceMask).map(x => x._1 && x._2)
      // Use OneHot to UInt to get the source Idx of this sink position
      val sourceIdx: UInt = OHToUInt(oneHot)
      // Matched
      val matched: Bool = VecInit(oneHot).asUInt().orR()
      // Assign
      io.sinks.map(_.bits).apply(idx) := Mux(matched, sourceData(sourceIdx), 0.U)
    }
  } else {
    require(requirement = false, "No element, fail")
  }
}

object MaskAggregator {

  def apply[T <: Data](data: Seq[T], mask: Seq[Bool]): (Seq[T], Seq[Bool]) = {
    // Width check
    val numElem: Int = data.length
    require(data.length == mask.length && data.nonEmpty)
    require(data.forall(_.getWidth > 0) && data.forall(_.getWidth == data.head.getWidth))

    if (data.length == 1) {
      val out:  T = Wire(chiselTypeOf(data.head))
      val bool: Bool = Wire(Bool())
      out := data.head
      bool := mask.head
      (Seq(out), Seq(bool))
    } else {
      val agg: MaskAggregator[T] =
        Module(new MaskAggregator[T](chiselTypeOf(data.head), numElem))
      agg.io.sources.zip(data).foreach { case (source, t) => source.bits := t }
      agg.io.sources.zip(mask).foreach { case (source, b) => source.valid := b }
      (agg.io.sinks.map(_.bits), agg.io.sinks.map(_.valid))
    }
  }

  def apply[T <: Data](data: Seq[T], mask: UInt): (Seq[T], Seq[Bool]) = apply(data, mask.asBools())
}
