package dsagen2.top.config

import chipsalliance.rocketchip.config._
import dsagen2.top.example._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile.BuildRoCC

class WithDevDSA
    extends Config((site, here, up) => {
      // Add DSAGen as an RoCC Accelerator
      case BuildRoCC =>
        up(BuildRoCC) ++ Seq((p: Parameters) => {
          val dsa = LazyModule(new DSAGenDev()(p ++ new WithDebuggable ++ new PrintADG))
          dsa
        })
      // Change the System Bus Width
      case SystemBusKey => up(SystemBusKey).copy(beatBytes = 32)
    })

class WithDemoDSA
  extends Config((site, here, up) => {
    // Add DSAGen as an RoCC Accelerator
    case BuildRoCC =>
      up(BuildRoCC) ++ Seq((p: Parameters) => {
        val dsa = LazyModule(new Demo()(p ++ new WithDebuggable ++ new PrintADG))
        dsa
      })
    // Change the System Bus Width
    case SystemBusKey => up(SystemBusKey).copy(beatBytes = 32)
  })

class WithMeshDSA
    extends Config((site, here, up) => {
      // Add DSAGen as an RoCC Accelerator
      case BuildRoCC =>
        up(BuildRoCC) ++ Seq((p: Parameters) => {
          val dsa = LazyModule(new DSAGenMesh()(p ++ new WithDebuggable ++ new PrintADG))
          dsa
        })
      // Change the System Bus Width
      case SystemBusKey => up(SystemBusKey).copy(beatBytes = 32)
    })

class WithMeshDSADualSPM
    extends Config((site, here, up) => {
      // Add DSAGen as an RoCC Accelerator
      case BuildRoCC =>
        up(BuildRoCC) ++ Seq((p: Parameters) => {
          val dsa = LazyModule(new DSA_DualSPM()(p ++ new WithDebuggable ++ new PrintADG))
          dsa
        })
      // Change the System Bus Width
      case SystemBusKey => up(SystemBusKey).copy(beatBytes = 32)
    })

class WithMesh4Gemm
    extends Config((site, here, up) => {
      // Add DSAGen as an RoCC Accelerator
      case BuildRoCC =>
        up(BuildRoCC) ++ Seq((p: Parameters) => {
          val dsa = LazyModule(new DSAGemm()(p ++ new WithDebuggable ++ new PrintADG))
          dsa
        })
      // Change the System Bus Width
      case SystemBusKey => up(SystemBusKey).copy(beatBytes = 32)
    })
