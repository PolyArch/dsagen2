package dsagen2.misc.module

import chisel3._

class SIntExtender(inWidth: Int, outWidth: Int) extends MultiIOModule {

  val input:  UInt = IO(Input(UInt(inWidth.W)))
  val output: UInt = IO(Output(UInt(outWidth.W)))

  val sinput:  SInt = input.asSInt()
  val soutput: SInt = WireInit(0.S(outWidth.W))
  soutput := sinput // will chisel do sign extending if outWidth < inWidth? Checked by Sihao: yes they did!
  output := soutput.asUInt()
}
