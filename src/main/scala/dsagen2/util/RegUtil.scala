package dsagen2.util

import chisel3._
import chisel3.util._

import scala.annotation.tailrec

object RegUtil {

  @tailrec
  def RegEnableN[T <: Data](d: T, e: Bool, stage: Int, acc: List[T] = Nil): List[T] = {
    require(stage >= 0, s"Number of stage should not be negative, but it is $stage")
    if (stage > 0) {
      val nxt: T = RegEnable(d, 0.U.asTypeOf(chiselTypeOf(d)), e)
      RegEnableN(nxt, e, stage - 1, acc :+ nxt)
    } else acc
  }

  @tailrec
  def RegNextN[T <: Data](d: T, stage: Int, acc: List[T] = Nil): List[T] = {
    require(stage >= 0, s"Number of stage should not be negative, but it is $stage")
    if (stage > 0) { val nxt: T = RegNext(d); RegNextN(nxt, stage - 1, acc :+ nxt) }
    else acc
  }
}
