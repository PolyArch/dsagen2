package dsagen2.top.dsa

import dsagen2.comp.diplomacy.CompBDirEdgeParameters
import dsagen2.comp.module.CompNodeModule
import dsagen2.sync.bundle.{IVPSetPort, OVPSetPort, VectorPortStatus}
import dsagen2.sync.config.{IVPNodeParameters, OVPNodeParameters}
import dsagen2.sync.module.{IVPNodeModule, OVPNodeModule}
import dsagen2.top.config.DSALink
import dsagen2.top.config.ParseKeys.{SyncEdges, SyncNodes}
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, InputVectorPort, OutputVectorPort}
import dsagen2.top.module.DSAGen
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait SyncBuild {
  dsagen: DSAGen =>

  /* ---------- Node Collection ---------- */

  // Collect all input vector port nodes
  def ivpNodeModules: Seq[IVPNodeModule] =
    filterDSANode(InputVectorPort).asInstanceOf[Seq[IVPNodeModule]]

  // Collect all output vector port nodes
  def ovpNodeModules: Seq[OVPNodeModule] =
    filterDSANode(OutputVectorPort).asInstanceOf[Seq[OVPNodeModule]]

  /* ---------- Statistic Collection ---------- */

  // Generate Name
  def syncSysName: String = s"IVP$numIVP.OVP$numOVP"

  // Collect all input vector port node parameters
  def ivpParams: Seq[IVPNodeParameters] = ivpNodeModules.map(_.ivpNode)

  // Collect all output vector port node parameters
  def ovpParams: Seq[OVPNodeParameters] = ovpNodeModules.map(_.ovpNode)

  // Collect the number of IVP
  def numIVP: Int = ivpNodeModules.length

  // Collect the number of OVP
  def numOVP: Int = ovpNodeModules.length

  def numVP: Int = numOVP + numIVP

  // Min and Max about VP
  def maxNumVP: Int = numIVP.max(numOVP)

  def minNumVP: Int = numIVP.min(numOVP)

  def moreIVP: Boolean = numIVP > numOVP

  def moreOVP: Boolean = !moreIVP

  // Get Node ID per node type
  def syncNodeIdByNodeType(vpNodeType: DSAGenNodeType): Int = {
    vpNodeType match {
      case InputVectorPort  => numIVP
      case OutputVectorPort => numOVP
      case errType: DSAGenNodeType =>
        require(requirement = false, s"Node Type $errType cannot be supported by Mem"); -1
      case _ => require(requirement = false, s"You are not providing nodeType"); -1
    }
  }

  /* ---------- Bundle Collection ---------- */

  // Collect the input vector ports status
  def getIVPsStatus: Seq[VectorPortStatus] = ivpNodeModules.sortBy(_.nodeId).map(_.module.ivpStatus)

  // Collect the output vector ports status
  def getOVPsStatus: Seq[VectorPortStatus] = ovpNodeModules.sortBy(_.nodeId).map(_.module.ovpStatus)

  // Collect the input vector port setting
  def getIVPSetting: Seq[IVPSetPort] = ivpNodeModules.sortBy(_.nodeId).map(_.module.ivpSetPort)

  // Collect the output vector port setting
  def getOVPSetting: Seq[OVPSetPort] = ovpNodeModules.sortBy(_.nodeId).map(_.module.ovpSetPort)

  // Generate IVP to connected compute modules mapping
  def ivpNeighborModules: mutable.Map[IVPNodeModule, Seq[CompNodeModule]] = {
    // Since the neighboring node of ivp can only be compute nodes, we should only build the mapping between
    // IVP and compute nodes
    val compModules: Seq[CompNodeModule] = compNodeModules
    // Get all of the Input Vector Modules
    val ivpModules: Seq[IVPNodeModule] = ivpNodeModules
    // Build the neighboring nodes mapping for all input vector port modules
    val nearIvpNodes: mutable.Map[IVPNodeModule, Seq[CompNodeModule]] = mutable.Map({
      // Loop over all input vector modules to get the neighboring connection
      ivpModules.map { currModule =>
        // Since we can only connect from IVP to Compute Modules, we only need to get sink modules for each ivp modules
        val sinkModules: Seq[CompNodeModule] = currModule.node.edges.out.flatMap { outEdge =>
          compModules.filter(otherModule => otherModule.node.edges.in.contains(outEdge))
        }
        // Build the mapping and distinct the neighboring nodes
        currModule -> sinkModules.distinct
      }
    }: _*)
    // Return the mapping
    nearIvpNodes
  }

  // Generate OVP to connected compute modules mapping
  def ovpNeighborModules: mutable.Map[OVPNodeModule, Seq[CompNodeModule]] = {
    // Get neighboring mapping between compute modules and output vector port node modules
    val compModules: Seq[CompNodeModule] = compNodeModules
    val ovpModules:  Seq[OVPNodeModule] = ovpNodeModules
    // Build the neighboring nodes mapping for all output vector port and compute nodes
    val nearOvpNodes: mutable.Map[OVPNodeModule, Seq[CompNodeModule]] = mutable.Map({
      // Loop over all output vector modules to get the neighboring connection
      ovpModules.map { currModule =>
        // Since we can only connect from Compute modules to OVP, we only need to get source modules for each ovp modules
        val sourceModules: Seq[CompNodeModule] = currModule.node.edges.in.flatMap { inEdge =>
          compModules.filter(otherModule => otherModule.node.edges.out.contains(inEdge))
        }
        // Build the mapping and distinct the neighboring nodes
        currModule -> sourceModules.distinct
      }
    }: _*)
    // Return the mapping
    nearOvpNodes
  }

  /* ---------- JSON Emitter ---------- */

  // Emit JSON for Sync Nodes
  def syncNodes2JSON: Seq[(String, JsObject)] = {
    val ivp2json = ivpNodeModules.map { module => s"${module.ivpNode.getNodeName}" -> module.toJSON }
    val ovp2json = ovpNodeModules.map { module => s"${module.ovpNode.getNodeName}" -> module.toJSON }
    ivp2json ++ ovp2json
  }

  // Get DSALink for IVP -> CompNode, CompNode -> OVP
  def syncDSALinks: List[DSALink] = {
    val accLinks: ListBuffer[DSALink] = ListBuffer[DSALink]()
    // Get all compute nodes
    val compModules: Seq[CompNodeModule] = dsagen.compNodeModules
    // loop over all input vector port modules
    for (ivpModule <- ivpNodeModules) {
      // Get all edges from ivp to compute node
      val outEdges: Seq[CompBDirEdgeParameters] = ivpModule.node.out.map(_._2)
      // Make sure all of this connection should be downward defined
      require(outEdges.forall(e => e.isDownward && !e.isUpward))
      // Loop over all output edges
      for ((outEdge, outIdx) <- outEdges.zipWithIndex) {
        // Get the compute node that is the sink of this edge
        val firstCompNodeCandidates: Seq[CompNodeModule] =
          compModules.filter(x => x.node.in.map(_._2).contains(outEdge))
        // Should only be one and only one compute node is using this edge
        require(firstCompNodeCandidates.length == 1)
        val firstCompModule: CompNodeModule = firstCompNodeCandidates.head
        // Calculate the input index
        val inIdx: Int = firstCompModule.inParams.indexWhere(t => t.equals(outEdge.dParam))
        require(
          firstCompModule.inParams.count(t => t.equals(outEdge.dParam)) == 1,
          s"First Compute Node that match IVP Downward Edge Parameter Number = " +
            s"${firstCompModule.inParams.count(t => t.equals(outEdge.dParam))}"
        )
        // Enqueue the Link Info
        accLinks += DSALink(
          ivpModule.ivpNode.nodeType,
          ivpModule.ivpNode.getNodeId,
          outIdx,
          firstCompModule.nodeType,
          firstCompModule.compNode.getNodeId,
          inIdx
        )
      }
    }
    // Loop over all output vector port modules
    for (ovpModule <- ovpNodeModules) {
      // Get all edges from compute modules to ovp
      val inEdges: Seq[CompBDirEdgeParameters] = ovpModule.node.in.map(_._2)
      // Make sure all inward edges is downward connected
      require(inEdges.forall(x => x.isDownward && !x.isUpward))
      // Loop over all input edges
      for ((inEdge, inIdx) <- inEdges.zipWithIndex) {
        // Get the compute node that is the source of this edge
        val lastCompNodeCadidates: Seq[CompNodeModule] =
          compModules.filter(x => x.node.out.map(_._2).contains(inEdge))
        // Should be one and only one compute provides this edge
        require(lastCompNodeCadidates.length == 1)
        val lastCompModule: CompNodeModule = lastCompNodeCadidates.head
        // Calculate the output index
        val outIdx: Int = lastCompModule.outParams.indexWhere(t => t.equals(inEdge.dParam))
        require(
          lastCompModule.outParams.count(t => t.equals(inEdge.dParam)) == 1,
          s"Last Compute Node that match OVP Downward Edge parameter number = " +
            s"${lastCompModule.outParams.count(t => t.equals(inEdge.dParam))}"
        )
        // Enqueue the Link Info
        accLinks += DSALink(
          lastCompModule.nodeType,
          lastCompModule.compNode.getNodeId,
          outIdx,
          ovpModule.nodeType,
          ovpModule.ovpNode.getNodeId,
          inIdx
        )
      }
    }
    accLinks.result()
  }

  // Emit JSON for Sync Nodes : IVP -> CompNode, CompNode -> OVP
  def syncEdges2JSON: List[JsObject] =
    syncDSALinks.map(x => x.toJSON)

  // Emit JSON for nodes and edges
  def sync2JSON: JsObject = Json.obj(
    SyncNodes.keyName -> JsObject(syncNodes2JSON),
    SyncEdges.keyName -> JsArray(syncEdges2JSON)
  )
}
