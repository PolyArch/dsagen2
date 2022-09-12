package dsagen2.sync.bundle

import chisel3._
import chisel3.util._
import dsagen2.sync.impl.VectorPortImpl

class VPRouteCfgBits(val outer: VectorPortImpl) extends Bundle {

  /* -------------------------      Hardware Fields      ------------------------- */

  // Route the output of ivp to correct place
  val xbarRoute: Vec[UInt] = Vec(outer.vecWidth, UInt(log2Ceil(outer.vecWidth + 1).W))

  // Enable bit that turn on this ivp
  val enabled: Bool = Bool()
}
