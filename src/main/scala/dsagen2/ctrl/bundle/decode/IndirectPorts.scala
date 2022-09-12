package dsagen2.ctrl.bundle.decode

import chisel3._
import dsagen2.top.config.DSAFixedConfig.MAX_LOCAL_VPORT_ID_BIT

/** Bundle used for parsing the indirect ports register
  */
class IndirectPorts extends Bundle {
  /* ---------- Hardware Fields ---------- */
  // Output vector port Id that provides Length 1D stream
  val length1DPortId: UInt = UInt(MAX_LOCAL_VPORT_ID_BIT.W)

  // Output vector port Id that provides Stride 2D stream
  val stride2DPortId: UInt = UInt(MAX_LOCAL_VPORT_ID_BIT.W)

  // Output vector port Id that provides Index stream
  val indexPortId: UInt = UInt(MAX_LOCAL_VPORT_ID_BIT.W)
}
