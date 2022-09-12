package dsagen2.sync.config

import chipsalliance.rocketchip.config.Config
import dsagen2.sync.config.SyncKeys.IVPNode
import dsagen2.top.config.enumeration.VPImplMode._

// Input Vector Port without XBar on the compute for better area overhead
class SimpleIVPConfig
    extends Config((site, here, up) => { case IVPNode =>
      IVPNodeParameters(vpImpl = NonXBarVP)
    })

// Input Vector Port with XBar on the compute side for better scheduling
class FullIVPConfig
    extends Config((site, here, up) => { case IVPNode =>
      IVPNodeParameters(vpImpl = FullXBarVP)
    })
