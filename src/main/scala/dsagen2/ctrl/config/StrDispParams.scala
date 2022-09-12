package dsagen2.ctrl.config

import chipsalliance.rocketchip.config.Config
import chisel3.util.{isPow2, log2Ceil}
import dsagen2.ctrl.config.CtrlKeys.CtrlNode

case class StrDispParams(
  dispQueueDepth: Int = 8,
  OoO_dispatch:   Boolean = true,
  reconfNetwork:  Boolean = true,
  // The number of stage from dispatcher to stream engine
  dispStage: Int = 2) {

  require(dispQueueDepth >= 2 && isPow2(dispQueueDepth))

  // Determine the number of bit for stream creation ID
  def maxStrCreationID: Int = dispQueueDepth * 2

  // Stream creation ID
  def strCreationIDBits: Int = log2Ceil(maxStrCreationID)
}

class WithDefaultStreamDispatcher
    extends Config((site, here, up) => { case CtrlNode =>
      StrDispParams()
    })
