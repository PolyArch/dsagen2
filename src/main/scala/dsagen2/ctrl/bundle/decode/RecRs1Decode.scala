package dsagen2.ctrl.bundle.decode

import chisel3._

class RecRs1Decode extends Bundle {
  /* ---------- Hardware Fields ---------- */
  val outputVPortId: UInt = UInt(7.W)
  val inputVPortId:  UInt = UInt(7.W)
}
