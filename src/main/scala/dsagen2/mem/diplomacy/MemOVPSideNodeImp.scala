package dsagen2.mem.diplomacy

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.mem.bundle.MemWriteBundle
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.OVPNodeParameters
import freechips.rocketchip.diplomacy.{InwardNodeImp, RenderedEdge}

object MemOVPSideNodeImp
    extends Object
    with InwardNodeImp[
      OVPNodeParameters,
      MemNodeParameters,
      OVP2MemParameter,
      MemWriteBundle
    ] {
  def edgeI(
    pd:         OVPNodeParameters,
    pu:         MemNodeParameters,
    p:          config.Parameters,
    sourceInfo: SourceInfo
  ): OVP2MemParameter =
    OVP2MemParameter(pd, pu, p, sourceInfo)

  def bundleI(ei: OVP2MemParameter): MemWriteBundle =
    Flipped(new MemWriteBundle(ei))

  def render(e: OVP2MemParameter): RenderedEdge =
    RenderedEdge(colour = "#FFFF00" /* red + green */, label = e.edgeLabel)
}
