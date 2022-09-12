package dsagen2.util

import chisel3._
import chisel3.util._



object UIntUtil {
  // Class helper for group bits as, a more chisel native way
  class vecUInt(val vecWidth : Int, val unitBits : Int) extends Bundle {
    val vecData : Vec[UInt] = Vec(vecWidth, UInt(unitBits.W))
  }

  /**
   * Split UInt into multiple narrower UInt as little-endian
   * @param u original UInt
   * @param unitBits the width of new UInt
   * @return a sequence of UInt
   */
  def groupBitsAs(u : UInt, unitBits : Int) : Seq[UInt] = {
    // Check width if width is known
    require(u.widthKnown, s"How to do bits group without knowing the bit width")
    if(u.widthKnown)
      require(u.getWidth % unitBits == 0, s"Bits grouping from ${u.getWidth}-bit to $unitBits-bit is not even")
    // To vector of UInt
    u.asTypeOf(new vecUInt(u.getWidth / unitBits, unitBits)).vecData
  }

  // Get largest UInt of width (all one's)
  def allOneUInt(width : Int) : UInt = if(width > 0) ((1 << width) - 1).U(width.W) else 0.U(1.W)

  // This function will return "next" with a 0-cycle delay when the "enable" signal is high. It's like a queue with
  // the "pipe" and "flow" parameters set to "true"
  def RegEnableThru[T <: Data](next: T, enable: Bool): T = {
    val buf = RegEnable(next, enable)
    Mux(enable, next, buf)
  }
}
