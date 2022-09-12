package dsagen2.comp.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.bundle.DirectionalConnection.{HasDownwardConnect, HasUpwardConnect}
import dsagen2.comp.config.CompNodeParameters
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, InputVectorPort, OutputVectorPort, ProcessingElement, Switch}
import dsagen2.util.NodeUtil.failGen

/** This is the edge parameter for connection among compute nodes OR between compute node and sync node
  *
  * There is no negotiation process actually, but only sanity check. There are at most two directional channel in this
  * bi-directional bundle. The downward bundle is purely defined by pd parameter, and upward bundle is completely defined
  * by pu parameter.
  *
  * This is EI/EO
  *
  * @param dCompNode  Parameter downward
  * @param uCompNode  Parameter upward
  * @param p          CDE parameter
  * @param sourceInfo chisel source code info
  */
case class CompBDirEdgeParameters(
  dCompNode:  CompNodeParameters,
  uCompNode:  CompNodeParameters,
  p:          Parameters,
  sourceInfo: SourceInfo) {
  // Create Edge Label
  def isDownward: Boolean = p(HasDownwardConnect)

  def isUpward: Boolean = p(HasUpwardConnect)

  def downwardChar: String = if (isDownward) "&gt;" else "-"

  def upwardChar: String = if (isUpward) "&lt;" else "-"

  def edgeLabel: String = {
    require(dCompNode.getNodeId >= 0 || uCompNode.getNodeId >= 0)
    val upNodeName:   String = dCompNode.getNodeName
    val downNodeName: String = uCompNode.getNodeName
    upNodeName + upwardChar + "-" + downwardChar + downNodeName
  }

  def downwardPair: (Int, Int) = (dCompNode.getNodeId, uCompNode.getNodeId)

  def upwardPair: (Int, Int) = (uCompNode.getNodeId, dCompNode.getNodeId)

  /* ---------- "Real" Negotiation (actually sanity check) ---------- */

  // Create bundle parameters based on boundary parameters

  val dParam: CompDirEdgeParameters = dCompNode.nodeType match {
    case ProcessingElement =>
      new CompDirEdgeParameters(enable = isDownward, compNode = dCompNode, p)
    case Switch =>
      // Connection between SW/PE <-> SW/PE
      new CompDirEdgeParameters(enable = isDownward, compNode = dCompNode, p)
    case InputVectorPort =>
      // Connection between IVP -> SW/PE
      require(dCompNode.isDummy)
      require(isDownward && !isUpward, "an ivp can only connect to comp node, not reversed")
      new CompDirEdgeParameters(enable = isDownward, compNode = uCompNode, p)
    case OutputVectorPort =>
      require(dCompNode.isDummy)
      require(isUpward && !isDownward, s"an ovp can only connect from comp node, not reversed")
      new CompDirEdgeParameters(enable = isUpward, uCompNode, p)
    case errType: DSAGenNodeType =>
      failGen(errType)
      new CompDirEdgeParameters(enable = false, dCompNode, p)
  }

  val uParam: CompDirEdgeParameters = uCompNode.nodeType match {
    case ProcessingElement =>
      new CompDirEdgeParameters(enable = isUpward, uCompNode, p)
    case Switch =>
      new CompDirEdgeParameters(enable = isUpward, uCompNode, p)
    case InputVectorPort =>
      require(uCompNode.isDummy)
      require(isUpward && !isDownward, "an ivp can only connect to comp node, not reversed")
      new CompDirEdgeParameters(enable = isUpward, dCompNode, p)
    case OutputVectorPort =>
      require(uCompNode.isDummy)
      require(!isUpward && isDownward, s"an ovp cannot connect from comp node, not reversed")
      new CompDirEdgeParameters(enable = isUpward, dCompNode, p)
    case errType: DSAGenNodeType =>
      failGen(errType)
      new CompDirEdgeParameters(enable = false, uCompNode, p)
  }
}
