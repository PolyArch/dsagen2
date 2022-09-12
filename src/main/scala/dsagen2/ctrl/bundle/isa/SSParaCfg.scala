package dsagen2.ctrl.bundle.isa

import chisel3._

/** Parsed bundle of Parameter Configuration Command
  */
class SSParaCfg extends SSISABundle {
  /* ---------- Hardware Fields ---------- */

  // Stickiness of second register
  // inst[31]
  val dsaReg2isStickiness: Bool = Bool()

  // Stickiness of first register
  // inst[30]
  val dsaReg1isStickiness: Bool = Bool()

  // Second DSA Register Number
  // inst[29:25]
  val dsaReg2: UInt = UInt(5.W)

  // CPU source register 2 that provides parameter for the second DSA register
  // inst[24:20]
  val cpuRs2: UInt = UInt(5.W)

  // CPU source register 1 that provides parameter for the first DSA register
  // inst[19:15]
  val cpuRs1: UInt = UInt(5.W)

  // Function 3-bit, not used
  // inst[14:12]
  val function3: UInt = UInt(3.W)

  // First DSA Register Number
  // inst[11:7]
  val dsaReg1: UInt = UInt(5.W)

  // Opcode
  // inst[6:0]
  val opcode: UInt = UInt(7.W)
}
