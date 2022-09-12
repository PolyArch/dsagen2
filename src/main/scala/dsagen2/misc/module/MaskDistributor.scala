package dsagen2.misc.module

import chisel3._
import chisel3.experimental._
import chisel3.internal.requireIsChiselType
import chisel3.util._
import dsagen2.misc.bundle.MaskArrangementIO

class MaskDistributor[T <: Data](
  val gen:     T,
  val numElem: Int
)(
  implicit compileOptions: chisel3.CompileOptions)
    extends MultiIOModule {
  require(numElem > 0)
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
  val io: MaskArrangementIO[T] = IO(new MaskArrangementIO(genType, numElem))

  // Wire
  val sourceData: Vec[T] = VecInit(io.sources.map(_.bits))
  val sourceMask: Vec[Bool] = VecInit(io.sources.map(_.valid))

  // Edge Case Judge
  if (numElem == 1) {
    io.sinks.head <> io.sources.head
  } else if (numElem > 1) {

    // Combination Logics

    // Assign the mask, distributor's mask is just forward
    io.sinks.map(_.valid).zip(sourceMask).foreach { case (valid, b) => valid := b }

    // Calculate the source Idx for each sink position
    for (idx <- 0 until numElem) {
      // Create source Idx wire
      val sourceIdx: UInt = WireInit(0.U(log2Ceil(numElem).W))
      if (idx > 0) {
        val preSourceMask: Seq[Bool] = sourceMask.slice(0, idx)
        sourceIdx := PopCount(preSourceMask)
      }
      // Assign from source
      io.sinks.map(_.bits).apply(idx) := Mux(sourceMask(idx), sourceData(sourceIdx), 0.U)
    }
  } else {
    require(requirement = false, "No element, fail")
  }
}

object MaskDistributor {

  def apply[T <: Data](data: Seq[T], mask: Seq[Bool]): (Seq[T], Seq[Bool]) = {
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
      val dis: MaskDistributor[T] =
        Module(new MaskDistributor[T](chiselTypeOf(data.head), numElem))
      dis.io.sources.zip(data).foreach { case (source, t) => source.bits := t }
      dis.io.sources.zip(mask).foreach { case (source, b) => source.valid := b }
      (dis.io.sinks.map(_.bits), dis.io.sinks.map(_.valid))
    }
  }

  def apply[T <: Data](vecData: Seq[ValidIO[T]]): Seq[ValidIO[T]] = {
    val dis: MaskDistributor[T] =
      Module(new MaskDistributor[T](chiselTypeOf(vecData.head.bits), vecData.length))
    dis.io.sources.zip(vecData).foreach { case (value, value1) => value <> value1 }
    dis.io.sinks
  }
}
