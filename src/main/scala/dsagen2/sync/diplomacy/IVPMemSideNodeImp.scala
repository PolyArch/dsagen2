package dsagen2.sync.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.Flipped
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.mem.bundle.MemReadBundle
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.Mem2IVPParameter
import dsagen2.sync.config.IVPNodeParameters
import freechips.rocketchip.diplomacy.{InwardNodeImp, RenderedEdge}

// Implementation of the Memory Side of Input Vector Port
object IVPMemSideNodeImp extends Object with InwardNodeImp[
  MemNodeParameters,  // DI: Memory to IVP
  IVPNodeParameters,  // UI: IVP to Memory
  Mem2IVPParameter,   // EI: Memory to IVP Edge Parameter
  MemReadBundle       // BI: Memory to IVP Bundle Type
] {

  /**
   * This function takes the IVP parameter and Memory Node parameter to generate the parameter for connection edge
   * This function is not the place that parameter got negotiated, just a wrapper
   * @param pd Memory Read Side parameter
   * @param pu IVP Enqueue Side parameter
   * @param p CDE Parameter
   * @param sourceInfo Chisel source info
   * @return Negotiated edge parameter
   */
  def edgeI(pd: MemNodeParameters, pu: IVPNodeParameters,
            p: Parameters, sourceInfo: SourceInfo): Mem2IVPParameter =
    Mem2IVPParameter(pd, pu, p, sourceInfo) // just pass argument to case class for negotiation

  // Generate Mem-IVP Bundle from Edge Parameter
  def bundleI(ei: Mem2IVPParameter): MemReadBundle =
    Flipped(new MemReadBundle(ei)) // just pass the argument, Bundle instantiation will do the job

  // Rendering the Inward Edge (each node is only responsible for rendering its inward edge)
  def render(e: Mem2IVPParameter): RenderedEdge =
    RenderedEdge(colour = "#00FF00" /* green */, label = e.edgeLabel)
}
