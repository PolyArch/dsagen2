package dsagen2.misc.module

import chisel3._
import chisel3.util._
import dsagen2.misc.util.ReduceUtil.extendMaskByExp

class MaskExpExtender(width: Int, numExp: Int) extends MultiIOModule {
  // Derived parameter
  val expBits: Int = if (numExp > 1) log2Ceil(numExp) else 0

  // IO
  val inputMask:  UInt = IO(Input(UInt(width.W)))
  val outputMask: UInt = IO(Output(UInt(width.W)))
  val exp:        Option[UInt] = if (expBits > 0) Some(IO(Input(UInt(expBits.W)))) else None

  // Output Connection
  outputMask := extendMaskByExp(inputMask, exp, numExp)
}

object MaskExpExtender {
  // Apply function
  def apply(mask: UInt, exp: Option[UInt], numExp: Int): UInt = extendMaskByExp(mask, exp, numExp)
}
