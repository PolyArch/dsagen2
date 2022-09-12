package dsagen2.misc.module

import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.util.MaskGen

class SmallMaskGen(memWidth: Int) extends MultiIOModule {
  val addr:   UInt = IO(Input(UInt((2 * log2Ceil(memWidth)).W)))
  val lgSize: UInt = IO(Input(UInt((2 * log2Ceil(memWidth)).W))) // I know this is incorrect
  val mask:   UInt = IO(Output(UInt(memWidth.W)))
  mask := MaskGen(addr, lgSize, memWidth)
}
