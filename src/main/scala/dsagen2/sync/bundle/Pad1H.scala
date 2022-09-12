package dsagen2.sync.bundle

import chisel3.{Bool, Bundle}

// Decode the padding mode number of one hot
class Pad1H extends Bundle {
  // Padding at the end of 1D stream
  val padOff1DEnd:  Bool = Bool()
  val padZero1DEnd: Bool = Bool()
  // Padding at the end of 2D stream
  val padOff2DEnd:  Bool = Bool()
  val padZero2DEnd: Bool = Bool()
  // Padding at the end of total stream
  val padOffStrEnd:  Bool = Bool()
  val padZeroStrEnd: Bool = Bool()
  // No padding
  val noPad: Bool = Bool()

  // Util
  def pad1D: Bool = padOff1DEnd || padZero1DEnd

  def pad2D: Bool = padOff2DEnd || padZero2DEnd

  def padStr: Bool = padOffStrEnd || padZeroStrEnd
}
