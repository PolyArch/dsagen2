package dsagen2.comp.config

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.CompKeys._
import dsagen2.comp.config.common.CompNodeOutputBufferParameters

class WithCompNode(node: CompNodeParameters, outputBufferDepth: Int = 4, staticOutputBuffer: Boolean = false)
    extends Config((site, here, up) => {
      // Node
      case CompNode => node
      // Common Output Buffer Parameters
      case OutputBuffer => CompNodeOutputBufferParameters(outputBufferDepth, staticOutputBuffer)
    })
