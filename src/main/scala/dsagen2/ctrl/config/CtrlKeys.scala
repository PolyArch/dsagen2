package dsagen2.ctrl.config

import chipsalliance.rocketchip.config.Field

case object CtrlKeys {
  // Key points to the stream dispatcher
  case object CtrlNode extends Field[StrDispParams]
}
