package dsagen2.ctrl.bundle

import chisel3._
import chisel3.util._
import dsagen2.ctrl.config.StrDispParams

/** Synchronization Entry from SSWait
  */
class SyncEntry(val ctrlParam: StrDispParams) extends Bundle {
  /* ---------- Hardware Fields ---------- */
  // Synchronization bit (16 bits)
  val syncMode:       Bool = Bool()
  val waitCompFinish: Bool = Bool()
  val waitAtomOp5:    Bool = Bool()
  val waitAtomOp4:    Bool = Bool()
  val waitAtomOp3:    Bool = Bool()
  val waitAtomOp2:    Bool = Bool()
  val waitAtomOp1:    Bool = Bool()
  val waitAtomOp0:    Bool = Bool()
  val waitWrite:      Bool = Bool()
  val waitRead:       Bool = Bool()
  var waitREG:        Bool = Bool()
  val waitDIS:        Bool = Bool()
  val waitGEN:        Bool = Bool()
  val waitREC:        Bool = Bool()
  val waitSPM:        Bool = Bool()
  val waitDMA:        Bool = Bool()
  // Signal value when wait command is retired
  val rdVal: UInt = UInt(1.W)
  // Signal value register to be written when command retired
  val rd: UInt = UInt(5.W)
  // Stream Command ID
  val strCreateID: UInt = UInt((ctrlParam.strCreationIDBits + 1).W)

  /* ---------- Utility ----------*/
  def isCommonMatch: Bool = syncMode

  def isExactMatch: Bool = !syncMode

  def memTypeMask: UInt = Cat(waitREG, waitDIS, waitGEN, waitREC, waitSPM, waitDMA)

  def memOpMask: UInt =
    Cat(waitAtomOp5, waitAtomOp4, waitAtomOp3, waitAtomOp2, waitAtomOp1, waitAtomOp0, waitWrite, waitRead)

  def isMemWait: Bool = memOpMask.orR() && memTypeMask.orR()

  def exactModeIllegal: Bool = isExactMatch && (
    (PopCount(memTypeMask) > 1.U && PopCount(
      memOpMask
    ) > 1.U) || // exact match cannot block more than one memory and one memOp
      (!isMemWait && !waitCompFinish) || // is Exact match but wait for nothing
      (isMemWait && waitCompFinish) // exact match cannot do mem and comp wait at the same time
  )

  def commonModeIllegal: Bool =
    isCommonMatch && !isMemWait && !waitCompFinish // Neither memory wait nor compute wait, then what you are waiting

  def illegal: Bool = exactModeIllegal || commonModeIllegal
}
