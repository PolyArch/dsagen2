package dsagen2.misc.bundle

import chisel3.util.ValidIO
import chisel3.{Bundle, Data, Flipped, Vec}

class MaskArrangementIO[T <: Data](private val gen: T, val numElem: Int) extends Bundle {
  val sources: Vec[ValidIO[T]] = Vec(numElem, Flipped(ValidIO(gen)))
  val sinks:   Vec[ValidIO[T]] = Vec(numElem, ValidIO(gen))
}
