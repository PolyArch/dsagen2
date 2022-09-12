package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.{CompNodeParameters, WithCompNode}
import dsagen2.top.diplomacy.DSANodeType.ProcessingElement

// Please assign a non negative node Id when use this config
class DefaultPEConfig(nodeId: Int = -1)
    extends Config(
      new WithRegister() ++
        new WithMetaControl() ++
        new WithDsaOperations() ++
        new WithCompNode(CompNodeParameters(nodeId = nodeId, nodeType = ProcessingElement))
    )
