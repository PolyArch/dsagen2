package dsagen2.comp.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.bundle.CompBDirBundle
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.diplomacy
import freechips.rocketchip.diplomacy._

// Comp Node Implementation
object CompNodeImp
    extends SimpleNodeImp[
      CompNodeParameters, // Downward flowing parameter
      CompNodeParameters, // Upward flowing parameter
      CompBDirEdgeParameters, // Parameter to describe the edge
      CompBDirBundle // The actual bundle between compute node
    ] {

  // Negotiate edge parameter
  def edge(
    pd:         CompNodeParameters,
    pu:         CompNodeParameters,
    p:          Parameters,
    sourceInfo: SourceInfo
  ): CompBDirEdgeParameters =
    diplomacy.CompBDirEdgeParameters(pd, pu, p, sourceInfo)

  // Create the hardware bundle
  def bundle(e: CompBDirEdgeParameters): CompBDirBundle = new CompBDirBundle(e)

  // Color of edge when rendering
  def render(e: CompBDirEdgeParameters): RenderedEdge =
    RenderedEdge(colour = "#0000FF" /* blue */, label = e.edgeLabel)
}
