package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.comp.config.CompKeys.{DsaOperations, MetaControl}
import dsagen2.comp.config.processing_element.PEMetaCtrlParameters

/** Processing Element Config Data Structure
  *
  * MetaCtrlLUT: The Meta Control Look Up Table
  * MetaCtrlMode: The Control Mode of Meta Control
  * RegistersConfig: The configuration of register (basically the register value)
  * ALUControl: ALU control data structure
  * ResultSubNet: deprecated, will be removed
  * Enabled: Whether this node is enabled or not
  *
  * @param p CDE parameter
  */
class PECfgBits(
  val numInput:  Int,
  val numOutput: Int
)(
  implicit val p: Parameters)
    extends Bundle {

  /* -------------------------      Extract Parameters      ------------------------- */

  val ctrlParam: Option[PEMetaCtrlParameters] = p.lift(MetaControl)
  val sizeCtrlLUT: Int =
    if (ctrlParam.isDefined && ctrlParam.get.supportControl) ctrlParam.get.sizeLUT else 0
  val instSlotSize: Int = p(DsaOperations).instSlotSize

  /* -------------------------      Hardware Fields      ------------------------- */

  // MSB

  // Bitmask State that mask out specific control key bits, limited by the size of control LUT
  // This Field is per lookup table, not per control entry. The bit width is hard coded by the max of following
  // For compare result, it is 3-bit (gt, eq, lt);
  // For stream state, 6-bit for (start/end of total stream, start/end of 2D stream, start/end of 1D stream)
  val bmss: Option[UInt] = if (sizeCtrlLUT > 0) Some(UInt(6.W)) else None

  // Control LUT, if supported, lutSize x entrySize, size of lut is from config
  val ctrlLUT: Option[Vec[PECtrlEntry]] =
    if (sizeCtrlLUT > 0) Some(Vec(sizeCtrlLUT, new PECtrlEntry)) else None

  // Instruction Slot
  val instSlot: Vec[PEFuncUnitInst] =
    Vec(instSlotSize, new PEFuncUnitInst(numInput, numOutput))

  // whether this node is enabled
  val enabled: Bool = Bool()

  // LSB
}
