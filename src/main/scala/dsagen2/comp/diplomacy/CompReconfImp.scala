package dsagen2.comp.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.config.reconf.{CompReconfEdge, CompReconfProto}
import dsagen2.top.bundle.ReconfPort
import freechips.rocketchip.diplomacy.{RenderedEdge, SimpleNodeImp}

object CompReconfImp
    extends SimpleNodeImp[
      CompReconfProto,
      CompReconfProto,
      CompReconfEdge,
      ReconfPort
    ] {
  // Generate the edge
  def edge(pd: CompReconfProto, pu: CompReconfProto, p: Parameters, sourceInfo: SourceInfo): CompReconfEdge = {
    CompReconfEdge(pd, pu, p, sourceInfo)
  }

  // Create the bundle
  def bundle(e: CompReconfEdge): ReconfPort = new ReconfPort(e)

  // Render Label
  def render(e: CompReconfEdge): RenderedEdge =
    RenderedEdge(colour = "#002400", label = "ReconfigEdge")
}
