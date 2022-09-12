package dsagen2.sync.config

import chipsalliance.rocketchip.config.Config
import dsagen2.sync.config.SyncKeys.OVPNode
import dsagen2.top.config.enumeration.VPImplMode.{FullXBarVP, NonXBarVP}

// Output Vector Port without XBar on the compute for better area overhead
class SimpleOVPConfig
    extends Config((site, here, up) => { case OVPNode =>
      OVPNodeParameters(vpImpl = NonXBarVP)
    })

// Output Vector Port with XBar on the compute side for better scheduling
class FullOVPConfig
    extends Config((site, here, up) => { case OVPNode =>
      OVPNodeParameters(vpImpl = FullXBarVP)
    })
