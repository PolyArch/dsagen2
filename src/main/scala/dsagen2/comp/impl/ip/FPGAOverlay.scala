package dsagen2.comp.impl.ip

import chipsalliance.rocketchip.config.{Config, Field}

case object FPGAOverlay extends Field[Boolean](false)

class WithDSAOverlay
    extends Config((site, here, up) => { case FPGAOverlay =>
      true
    })
