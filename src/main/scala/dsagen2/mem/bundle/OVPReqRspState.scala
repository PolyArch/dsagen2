package dsagen2.mem.bundle

import chisel3._

class OVPReqRspState extends Bundle {
  val responseValid: Bool = Bool()
  val requestReady:  Bool = Bool()

  def idle: Bool = !responseValid && !requestReady

  def requested: Bool = requestReady && !responseValid

  def responded: Bool = responseValid && !requestReady

  def illegal: Bool = responseValid && requestReady
}
