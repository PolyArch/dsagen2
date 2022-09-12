package dsagen2.ctrl.bundle.isa

import chisel3._
import dsagen2.top.config.DSAFixedConfig.MAX_LOCAL_VPORT_ID_BIT

/** Parsed bundle of vector port configuration command
  */
class SSPortCfg extends SSISABundle {

  /* ---------- Hardware Fields ---------- */

  // Specifying which output vector port we receive value from
  // inst[31:25]
  val targetVPIdx: UInt = UInt(MAX_LOCAL_VPORT_ID_BIT.W)

  // CPU Source Register that provides value for vector port settings
  // inst[24:20]
  val cpuRs2: UInt = UInt(5.W)

  // CPU Source Register that provides value for vector port settings
  // inst[19:15]
  val cpuRs1: UInt = UInt(5.W)

  // function 3
  // inst[14:12]
  val function3: UInt = UInt(3.W)

  // Tell the direction of the vector port
  // inst[24]
  val isOVP: Bool = Bool()

  // Specifying which configuration register of vector port should be update
  // inst[23:20]
  val vpSetRegIdx: UInt = UInt(4.W)

  // Opcode
  // inst[6:0]
  val opcode: UInt = UInt(7.W)

  /* ---------- Utility ---------- */

  // Get the direction of vector port
  def isIVP: Bool = !isOVP
}
