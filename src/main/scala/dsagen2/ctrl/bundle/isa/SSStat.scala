package dsagen2.ctrl.bundle.isa

import chisel3._

class SSStat extends SSISABundle {

  /* ---------- Hardware Fields ---------- */

  // Not used
  // inst[31:20]
  val EMPTY: UInt = UInt((7 + 5).W)

  // CPU Register Number that provides the encoded information for collecting
  // inst[19:15]
  val cpuRs1: UInt = UInt(5.W)

  // Function3
  // inst[14:12]
  val function3: UInt = UInt(3.W)

  // Destination Register that receive the synchronization signal
  // inst[11:7]
  val cpuRd: UInt = UInt(5.W)

  // Opcode
  // inst[6:0]
  val opcode: UInt = UInt(7.W)
}
