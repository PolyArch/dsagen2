package dsagen2.sync.diplomacy

import chipsalliance.rocketchip.config
import chisel3.Flipped
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.{bundle, diplomacy}
import dsagen2.comp.bundle.CompBDirBundle
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.diplomacy.CompBDirEdgeParameters
import freechips.rocketchip.diplomacy.{InwardNodeImp, RenderedEdge}

object OVPCompSideNodeImp extends Object with InwardNodeImp[
  CompNodeParameters,
  CompNodeParameters,
  CompBDirEdgeParameters,
  CompBDirBundle
]{
  // Negotiate Compute Node to Output Vector Port Parameter to generate Edge Parameter
  def edgeI(pd: CompNodeParameters, // pd is from compute node to output vector port
            pu: CompNodeParameters, // pu is from output vector port to compute node
            p: config.Parameters, sourceInfo: SourceInfo): CompBDirEdgeParameters = {
    // This is the edge negotiation process for Compute node to output vector port
    // Since the connection between compute node to output vector port is completely defined by compute node (only pd)
    // So we just pass the argument
    diplomacy.CompBDirEdgeParameters(pd, pu, p, sourceInfo)
  }

  // Generate Compute Node to Output Vector Port Bundle
  def bundleI(ei: CompBDirEdgeParameters): CompBDirBundle = Flipped(new CompBDirBundle(ei))

  // Rendering the Input Edge
  def render(e: CompBDirEdgeParameters): RenderedEdge =
    RenderedEdge(colour = "#FF0000" /*red*/, label = e.edgeLabel)
}
