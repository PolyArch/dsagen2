package dsagen2.sync.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.bundle.CompBDirBundle
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.diplomacy
import dsagen2.comp.diplomacy.CompBDirEdgeParameters
import freechips.rocketchip.diplomacy.OutwardNodeImp

// Implementation of the Compute Side of Input Vector Port
object IVPCompSideNodeImp extends Object with OutwardNodeImp[
  CompNodeParameters,     // DO: IVP to Compute
  CompNodeParameters,     // UO: Compute to IVP
  CompBDirEdgeParameters, // EO: IVP to Compute Edge Parameter
  CompBDirBundle          // BO: IVP to Compute Bundle Type
] {
  // Negotiate the generate the IVP-Comp Edge Parameter
  def edgeO(pd: CompNodeParameters, pu: CompNodeParameters,
            p: Parameters, sourceInfo: SourceInfo): CompBDirEdgeParameters =
    // Just pass arguments to the BDirEdge parameter case class and it will handle negotiation
    diplomacy.CompBDirEdgeParameters(pd, pu, p, sourceInfo)

  // Generate IVP-Comp Bundle from Edge Parameter
  def bundleO(ep: CompBDirEdgeParameters): CompBDirBundle = new CompBDirBundle(ep)
}
