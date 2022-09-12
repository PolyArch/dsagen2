package dsagen2.ctrl.bundle.isa

import chisel3._

/** Parsed bundle of Linear Stream instantiation command
  */
class SSLinStrm extends SSISABundle {
  /* ---------- Hardware Fields ---------- */

  // The highest 12 bits is not used, originally it is use for encoding target port, but later it is moved to
  // rs1 value

  // NOT used 12 bits
  // inst[31:20]
  val EMPTY: UInt = UInt(12.W)

  // CPU source register 1 that provides encoded parameter of linear stream
  // inst[19:15]
  val cpuRs1: UInt = UInt(5.W)

  // Function 3-bit, not used
  // inst[14:12]
  val function3: UInt = UInt(3.W)

  // CPU destination register, not used in linear stream instantiation command
  // inst[11:7]
  val cpuRd: UInt = UInt(5.W)

  // Opcode
  // inst[6:0], should be 1, 2, 3
  val opcode: UInt = UInt(7.W)
}
