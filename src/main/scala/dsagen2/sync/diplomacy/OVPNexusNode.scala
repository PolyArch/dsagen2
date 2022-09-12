package dsagen2.sync.diplomacy

import dsagen2.comp.config.CompNodeParameters
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.OVPNodeParameters
import freechips.rocketchip.diplomacy.{MixedNexusNode, ValName}

case class OVPNexusNode(comp2memFn: Seq[CompNodeParameters] => OVPNodeParameters,
                        mem2compFn: Seq[MemNodeParameters] => CompNodeParameters)(implicit valName : ValName)
  extends MixedNexusNode(OVPCompSideNodeImp, OVPMemSideNodeImp)(
    dFn = comp2memFn,
    uFn = mem2compFn)
