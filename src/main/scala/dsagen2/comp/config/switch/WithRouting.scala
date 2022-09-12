package dsagen2.comp.config.switch

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.CompKeys._

class WithRouting(
  initFullMatrix:    Array[Array[Boolean]] = Array.empty,
  initIndividualMat: Array[Array[Boolean]] = Array.empty)
    extends Config((site, here, up) => { case SwitchRouting =>
      SWRoutingParameters(initFullMatrix, initIndividualMat)
    })
