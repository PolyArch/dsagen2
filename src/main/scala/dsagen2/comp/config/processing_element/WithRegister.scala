package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.CompKeys._

class WithRegister(num_register: Int = 1)
    extends Config((site, here, up) => { case RegFile =>
      PERegFileParameters(num_register)
    })
