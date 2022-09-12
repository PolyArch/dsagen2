package dsagen2.sync.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.top.config.DSAFixedConfig
import dsagen2.top.config.DSAFixedConfig.{NUM_IVP_REG, setBroadcast}
import freechips.rocketchip.tile.XLen

class IVPSetPort(implicit val p: Parameters) extends Bundle {
  /* ------------------------- Extract Parameters ------------------------- */

  /* ------------------------- Derived Parameters ------------------------- */

  /* ------------------------- Hardware Fields    ------------------------- */

  // Valid and Reset
  val valid: Bool = Bool()
  val reset: Bool = Bool()
  // VP Register Index
  val vpRegIdx: UInt = UInt(log2Ceil(NUM_IVP_REG).W)

  // VP Register Value
  val vpRegVal: UInt = UInt(p(XLen).W)

  /* ------------------------- Utility Function   ------------------------- */
  // This command is used for set the target vector port to broadcast another stream, broadcasting will be finished
  // when the other stream ends
  def isBroadcastIVPortIdSet: Bool = vpRegIdx === setBroadcast && valid

  // This command will enable repeat port function at the input vector port to repeat vector input; This repeat function
  // will be turn off when the repeat time is equal to zero
  def isRepeatTimeSet: Bool = vpRegIdx === DSAFixedConfig.setRepeatTime && valid

  // This command will also enable the repeat port function at the input vector port to repeat vector input; This
  // repeat function will be turn off and delta of repeat time will be reset to zero
  def isRepeatDeltaSet: Bool = vpRegIdx === DSAFixedConfig.setRepeatDelta && valid
}
