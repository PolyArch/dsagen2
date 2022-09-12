package dsagen2.sync.bundle

import chisel3._

/** Data Structure to keep track of the Length of inner most loop, together with the state of current 1D stream (state
  * of higher dimension)
  *
  * @param l1dBits The number of bits needed to describe the length1D
  */
class Length1DState(val l1dBits: Int) extends Bundle {
  val length1D: UInt = UInt(l1dBits.W)
  val end2D:    Bool = Bool()
  val start2D:  Bool = Bool()
  val endStr:   Bool = Bool()
  val startStr: Bool = Bool()
}
