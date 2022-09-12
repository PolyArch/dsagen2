package dsagen2.mem.diplomacy

import chipsalliance.rocketchip.config
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.mem.bundle.MemReadBundle
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.IVPNodeParameters
import freechips.rocketchip.diplomacy.OutwardNodeImp

// Node Implementation of the input vector port side of memory node
object MemIVPSideNodeImp
    extends Object
    with OutwardNodeImp[
      MemNodeParameters, // DO: Memory to IVP
      IVPNodeParameters, // UO: IVP to Memory
      Mem2IVPParameter, // EO: Memory to IVP Edge Parameter
      MemReadBundle // BO: Memory to IVP Bundle Type
    ] {
  def edgeO(
    pd:         MemNodeParameters,
    pu:         IVPNodeParameters,
    p:          config.Parameters,
    sourceInfo: SourceInfo
  ): Mem2IVPParameter =
    Mem2IVPParameter(pd, pu, p, sourceInfo) // just pass argument to case class for negotiation

  def bundleO(eo: Mem2IVPParameter): MemReadBundle =
    new MemReadBundle(eo) // just pass the argument, Bundle instantiation will do the job
}
