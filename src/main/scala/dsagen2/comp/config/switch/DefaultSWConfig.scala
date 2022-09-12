package dsagen2.comp.config.switch

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.{CompNodeParameters, WithCompNode}
import dsagen2.top.diplomacy.DSANodeType.Switch

class DefaultSWConfig
    extends Config(
      new WithRouting() ++
        new WithCompNode(CompNodeParameters(nodeType = Switch, compUnitBits = 64), 4)
    )
