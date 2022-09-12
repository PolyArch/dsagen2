package dsagen2.sync.diplomacy

import chipsalliance.rocketchip.config
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.mem.bundle.MemWriteBundle
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.OVP2MemParameter
import dsagen2.sync.config.OVPNodeParameters
import freechips.rocketchip.diplomacy.OutwardNodeImp

object OVPMemSideNodeImp extends Object with OutwardNodeImp[
  OVPNodeParameters,
  MemNodeParameters,
  OVP2MemParameter,
  MemWriteBundle
]{
  // Negotiate Output Vector Port to Memory Node Parameter to generate Edge Parameter
  def edgeO(ovpNode: OVPNodeParameters, memNode: MemNodeParameters,
            p: config.Parameters, sourceInfo: SourceInfo): OVP2MemParameter = {
    // Pass the arguments directly
    OVP2MemParameter(ovpNode, memNode, p, sourceInfo)
  }

  // Generate Bundle Parameter from Edge Parameter
  def bundleO(eo: OVP2MemParameter): MemWriteBundle =
    // Just pass the argument to bundle instantiation
    new MemWriteBundle(eo)
}
