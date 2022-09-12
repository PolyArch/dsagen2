package dsagen2.mem.config

import chipsalliance.rocketchip.config.Config
import dsagen2.mem.config.MemKeys.MemNode

// Default DMA Config
class DefaultDMAConfig
    extends Config((site, here, up) => { case MemNode =>
      MemNodeParameters.DMA
    })

// Default Scratchpad Config
class DefaultSPMConfig
    extends Config((site, here, up) => { case MemNode =>
      MemNodeParameters.SPM
    })

// Affine Scratchpad Config
class AffineSPMConfig
    extends Config((site, here, up) => { case MemNode =>
      MemNodeParameters.SPM.turnOffIndirect
    })

// Default Generate Engine Config
class DefaultGENConfig
    extends Config((site, here, up) => { case MemNode =>
      MemNodeParameters.GEN
    })

// Default Register Config
class DefaultREGConfig
    extends Config((site, here, up) => { case MemNode =>
      MemNodeParameters.REG
    })

// Default Recurrence Config
class DefaultRECConfig
    extends Config((site, here, up) => { case MemNode =>
      MemNodeParameters.REC
    })
