package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3.Wire
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.diplomacy.{CompBDirEdgeParameters, CompDirEdgeParameters}
import dsagen2.top.diplomacy.DSANodeType.OutputVectorPort
import freechips.rocketchip.diplomacy.MixedNode

/** Because the Bundle Type is driven by the Source Compute Node, so for the input side of each node. The granularity
  * may be different from the one of itself. So this trait is able to convert the input bundles to its own granularity
  */
trait FormalizeCompBDirBundle {

  // Node Parameter
  implicit val p: Parameters
  val compNode:   CompNodeParameters

  // Node for Comp LazyModule
  val node: MixedNode[
    CompNodeParameters,
    CompNodeParameters,
    CompBDirEdgeParameters,
    CompBDirBundle,
    CompNodeParameters,
    CompNodeParameters,
    CompBDirEdgeParameters,
    CompBDirBundle
  ]

  /** Convert the conceptual Input/Output (Parameter flow direction) to actual input/output bundle
    * Output bundle: Parameters are all defined by current node, so not changed
    * Input bundle: except connection from vector port, rest are defined by other nodes, so transform needed
    */
  // Collect output/input bundle
  lazy val (output_bundles, outParams, rawInputBundles, inParams): (
    Seq[CompDirBundle],
    Seq[CompDirEdgeParameters],
    Seq[CompDirBundle],
    Seq[CompDirEdgeParameters]
  ) = {
    val out_downward_bundle_edge = node.out.filter { case (_, out_edge) => out_edge.dParam.enable }
    val in_upward_bundle_edge = node.in.filter { case (_, in_edge) => in_edge.uParam.enable }
    val out_upward_bundle_edge = node.out.filter { case (_, out_edge) => out_edge.uParam.enable }
    val in_downward_bundle_edge = node.in.filter { case (_, in_edge) => in_edge.dParam.enable }

    // Return Calculated Result
    val (tempOutB, tempOutE, tempInB, tempInE) = (
      // Output Bundles
      (out_downward_bundle_edge.map(_._1.dPort) ++ in_upward_bundle_edge.map(_._1.uPort))
        .filter(_.isDefined)
        .map(_.get),
      // Output Edges Parameters
      out_downward_bundle_edge.map(_._2.dParam) ++ in_upward_bundle_edge.map(_._2.uParam),
      // Input Bundles
      (in_downward_bundle_edge.map(_._1.dPort) ++ out_upward_bundle_edge.map(_._1.uPort))
        .filter(_.isDefined)
        .map(_.get),
      // Input Edges Parameters
      in_downward_bundle_edge.map(_._2.dParam) ++ out_upward_bundle_edge.map(_._2.uParam)
    )

    // Sanity Check
    require(
      tempOutB.length == tempOutE.length,
      s"#Output Bundle (${tempOutB.length}) != #Output Edge (${tempOutE.length})"
    )
    require(tempInB.length == tempInE.length, s"#Input Bundle (${tempInB.length}) != #Input Edge (${tempInE.length})")
    (tempOutB, tempOutE, tempInB, tempInE)
  }

  // [[rawInputBundles]] is defined by using other nodes, so transformation needed
  lazy val input_bundles: Seq[CompDirBundle] = {
    compNode.nodeType match {
      case OutputVectorPort =>
        // Output Vector Port bundle is defined by other nodes, so no transformation needed
        rawInputBundles
      case _ =>
        rawInputBundles.map { originalInBundle =>
          val transInBundle: CompDirBundle = Wire(
            new CompDirBundle(
              new CompDirEdgeParameters(originalInBundle.param.enable, compNode, p)
            )
          )
          transInBundle.valid.get := originalInBundle.valid.get
          originalInBundle.ready.get := transInBundle.ready.get
          (transInBundle.ctrl.get.dAct, originalInBundle.ctrl.get.dAct) match {
            case (Some(sink), Some(source)) => sink := source
            case _                          => require(requirement = false, s"Translation should not add/delete signal")
          }
          (originalInBundle.ctrl.get.uAct, transInBundle.ctrl.get.uAct) match {
            case (Some(sink), Some(source)) => sink := source
            case _                          => require(requirement = false, s"Translation should not add/delete signal")
          }
          CompDataConnect(transInBundle.data.get, originalInBundle.data.get)
          transInBundle
        }
    }
  }

  def numInput: Int = inParams.length

  def numOutput: Int = outParams.length
}
