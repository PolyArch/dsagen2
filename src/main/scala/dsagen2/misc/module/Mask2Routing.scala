package dsagen2.misc.module

import chisel3._
import chisel3.util._

/** This module calculate the output routing based on mask
  * If mask is input mask:  mask = [1, 1, 0, 1] -> routing = [0, 4, 3, 1]
  * If mask is output mask: mask = [1, 1, 0, 1] -> routing = [3, 2, 0, 1]
  * Please be attention, for the routing selection = 0, it means selecting no where
  */
class Mask2Routing(val maskBits: Int, val asInput: Boolean) extends MultiIOModule {
  // Requirement
  require(maskBits > 0, s"mask bits should be positive, but it is $maskBits")
  // IO
  val mask:       UInt = IO(Input(UInt(maskBits.W)))
  val outRouting: Vec[UInt] = IO(Output(Vec(maskBits, UInt(log2Ceil(maskBits + 1).W))))
  if (maskBits == 1) {
    outRouting.head := mask.asBool()
  } else {
    // Calculate the same side routing
    // Calculate the destination index for each input
    val rawSameSideRouting: Seq[UInt] =
      mask
        .asBools()
        .drop(1)
        .map(_.asUInt())
        .scanLeft(mask.asBools().head.asUInt())((low, high) => low +& high)
    require(rawSameSideRouting.length == maskBits)
    // Mask off the input routing with mask by replacing non-valid with zero
    val sameSideRouting: Seq[UInt] = rawSameSideRouting.zip(mask.asBools()).map { case (sameSideIdx, valid) =>
      Mux(valid, sameSideIdx, 0.U)
    }

    if (asInput) {
      // Input is not the same side with [[outRouting]], so Mux1H is needed

      // Tag each output with index starting from 1
      val inputIdx: Seq[UInt] = (1 to maskBits).map(_.asUInt())
      // Compare between input and output to get One Hot for mux
      val outOHs: Seq[UInt] = for (outIdx <- 1 to maskBits) yield {
        VecInit(sameSideRouting.map(i => i === outIdx.U)).asUInt()
      }
      // Connect the input idx to output with MuxOH
      require(outRouting.length == outOHs.length)
      require(outRouting.length == inputIdx.length)
      outRouting.zip(outOHs).foreach { case (out, oh) =>
        out := Mux1H(oh, inputIdx)
      }
    } else {
      // Mask is output mask which is the same side with the output side
      require(outRouting.length == sameSideRouting.length)
      // Connect to output routing
      outRouting.zip(sameSideRouting).foreach { case (out, routing) =>
        out := routing
      }
    }
  }
}

object Mask2Routing {

  /** Convert input mask to output routing
    *
    * @param mask Mask as the other side of routing
    * @return The other side of routing
    */
  def inputMask2outRouting(mask: UInt): Seq[UInt] = {
    val m2r: Mask2Routing = Module(new Mask2Routing(mask.getWidth, asInput = true))
    m2r.mask := mask
    m2r.outRouting
  }

  /** Convert the mask as Sequence of bool to the other side routing
    *
    * @param mask Mask as the other side of routing
    * @return The other side of routing
    */
  def inputMask2outRouting(mask: Seq[Bool]): Seq[UInt] = {
    inputMask2outRouting(VecInit(mask).asUInt())
  }

  /** Convert output mask to output routing
    *
    * @param mask Mask as the same side of routing
    * @return The same side of routing
    */
  def outputMask2outRouting(mask: UInt): Seq[UInt] = {
    val m2r: Mask2Routing = Module(new Mask2Routing(mask.getWidth, asInput = false))
    m2r.mask := mask
    m2r.outRouting
  }

  /** Convert the mask as Sequence of bool to the same side routing
    *
    * @param mask Mask as the same side of routing
    * @return The same side of routing
    */
  def outputMask2outRouting(mask: Seq[Bool]): Seq[UInt] = {
    outputMask2outRouting(VecInit(mask).asUInt())
  }
}
