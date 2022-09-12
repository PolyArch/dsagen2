package dsagen2.sync.diplomacy

import dsagen2.comp.config.CompNodeParameters
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.IVPNodeParameters
import freechips.rocketchip.diplomacy._

case class IVPNexusNode(mem2compFn : Seq[MemNodeParameters] => CompNodeParameters,
                        comp2memFn : Seq[CompNodeParameters] => IVPNodeParameters)(implicit valName : ValName)
  extends MixedNexusNode(IVPMemSideNodeImp, IVPCompSideNodeImp)(dFn = mem2compFn, uFn = comp2memFn)

