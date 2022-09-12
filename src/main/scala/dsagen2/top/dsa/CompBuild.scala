package dsagen2.top.dsa

import dsagen2.comp.bundle.CompNodeStatus
import dsagen2.comp.diplomacy.CompBDirEdgeParameters
import dsagen2.comp.module.CompNodeModule
import dsagen2.top.config.DSALink
import dsagen2.top.config.ParseKeys.{CompEdges, CompNodes}
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, InputVectorPort, OutputVectorPort, ProcessingElement, Switch}
import dsagen2.top.diplomacy.ReconfNode
import dsagen2.top.module.DSAGen
import dsagen2.top.topology.CompMeshLikeTopology
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.collection.Set
import scala.collection.mutable.ListBuffer

/* Compute System Building Environment */
trait CompBuild extends CompMeshLikeTopology {
  dsagen: DSAGen =>

  var reconfDepth: Int = -1

  /* ---------- Node Collection ---------- */

  // Private a specific kind of compute node modules based on given node type
  private def compNodeModules(compNodeTypes: DSAGenNodeType*): Seq[CompNodeModule] =
    filterDSANode(compNodeTypes: _*).asInstanceOf[Seq[CompNodeModule]]

  // Collect all compute nodes
  def compNodeModules: Seq[CompNodeModule] = compNodeModules(ProcessingElement, Switch)

  // Collect all pe nodes
  private def peModules: Seq[CompNodeModule] = compNodeModules(ProcessingElement)

  // Collect all sw nodes
  private def swModules: Seq[CompNodeModule] = compNodeModules(Switch)

  /* ------------------------------ Statistic Collection ------------------------------ */

  // Generate Name
  def compSysName: String = {
    s"${if (numPE > 0) s"PE$numPE-$getAllOpDataTypeName" else ""}" +
      s".${if (numSW > 0) s"SW$numSW" else ""}"
  }

  def numPE: Int = peModules.length

  def numSW: Int = swModules.length

  def numCOMP: Int = compNodeModules.length

  // Get Node Id per node Type
  def compNodeIdByNodeType(memNodeType: DSAGenNodeType): Int = {
    memNodeType match {
      case ProcessingElement => numPE
      case Switch            => numSW
      case errType: DSAGenNodeType =>
        require(requirement = false, s"Node Type $errType cannot be supported by Mem"); -1
      case _ => require(requirement = false, s"You are not providing nodeType"); -1
    }
  }

  // Get the set of operation + data type from all PEs
  def getAllOpDataType: Set[DsaOperDataType] = peModules.map(_.opDataTypeSet).reduce(_ ++ _)

  // Get the compact name of all operation+data type set
  def getAllOpDataTypeName: String = getAllOpDataType.compactName

  /* ---------- JSON Emitter ---------- */

  // Emit JSON for Compute Nodes
  def compNodes2JSON: Seq[(String, JsObject)] =
    compNodeModules.map { module => s"${module.compNode.getNodeName}" -> module.toJSON }

  // Get DSALinks for Comp <-> Comp
  def compDSALinks: List[DSALink] = {
    // Get every input edges and output edges
    val sourceModuleNodes = compNodeModules.map(source => (source, source.node, source.compNode))
    val sinkModuleNodes = compNodeModules.map(sink => (sink, sink.node, sink.compNode))
    val accLinks: ListBuffer[DSALink] = ListBuffer[DSALink]()

    // Loop over all outward diplomatic connection of the source nodes
    for ((sourceModule, sourceDpNode, sourceCompNode) <- sourceModuleNodes) {
      // Get all outward edges
      val outEdges: Seq[CompBDirEdgeParameters] = sourceDpNode.out.map(_._2)
      // Loop over all outward edges
      for (outEdge <- outEdges) {
        // Get the sink node whose inward edges contains this outward edges
        val sinkNodeCandidates = sinkModuleNodes.filter(sink => sink._2.in.map(_._2).contains(outEdge))
        // There should be just one compute node that is the sink of this outward edge, or the sink is OVP
        require(sinkNodeCandidates.length == 1 || outEdge.uCompNode.nodeType == OutputVectorPort)
        // Connection between compute node and sync node is not handled here
        if (outEdge.uCompNode.nodeType != OutputVectorPort) {
          // Get the sink node
          val (sinkModule, sinkDiploNode, sinkCompNode) = sinkNodeCandidates.head
          // Get the physical port (directional) index
          if (outEdge.dParam.enable) {

            // TODO: Please check the [[CompDirEdgeParameters]], there is a ugly design for uniquely label the
            //  CompDirEdgeParameters by override the hascode and equal function, please fix it later
            //  same for the sinkModules

            // Find the index of physical output port
            val outIdx: Int = sourceModule.outParams.indexWhere(t => t eq outEdge.dParam)
            require(sourceModule.outParams.count(t => t eq outEdge.dParam) == 1)

            // Find the index of physical input port
            val inIdx: Int = sinkModule.inParams.indexWhere(t => t.equals(outEdge.dParam))
            require(
              sinkModule.inParams.count(t => t.equals(outEdge.dParam)) == 1,
              s"Sink Input match Source Output Parameter number = " +
                s"${sinkModule.inParams.count(t => t.equals(outEdge.dParam))}"
            )

            // Construct DSA Link object
            accLinks += DSALink(
              sourceCompNode.nodeType,
              sourceCompNode.getNodeId,
              outIdx,
              sinkCompNode.nodeType,
              sinkCompNode.getNodeId,
              inIdx
            )
          }
        }
      }
    }

    // Loop over all inward diplomatic connection of sink nodes
    for ((sinkModule, sinkDpNode, sinkCompNode) <- sinkModuleNodes) {
      // Get all inward edges
      val inEdges: Seq[CompBDirEdgeParameters] = sinkDpNode.in.map(_._2)
      // Loop over all inward edges
      for (inEdge <- inEdges) {
        // Get the source node whose outward edges contains this inward edge
        val sourceNodeCadidates = sourceModuleNodes.filter(source => source._2.out.map(_._2).contains(inEdge))
        // The number of candidates should be just one
        require(sourceNodeCadidates.length == 1 || inEdge.dCompNode.nodeType == InputVectorPort)
        // Connection between compute node and ivp node is not handled here
        if (inEdge.dCompNode.nodeType != InputVectorPort) {
          // Get the source node
          val (sourceModule, sourceDpNode, sourceCompNode) = sourceNodeCadidates.head
          // Get the physical port (directional) index
          if (inEdge.uParam.enable) {

            // Find the index of physical output port
            val outIdx: Int = sinkModule.outParams.indexWhere(t => t eq inEdge.uParam)
            require(
              sinkModule.outParams.count(t => t eq inEdge.uParam) == 1,
              s"Number of match between sink outParams and inEdge.uParam = " +
                s"${sinkModule.outParams.count(t => t eq inEdge.uParam)}"
            )

            // Find the index of physical input port
            val inIdx: Int = sourceModule.inParams.indexWhere(t => t.equals(inEdge.uParam))
            require(
              sourceModule.inParams.count(t => t.equals(inEdge.uParam)) == 1,
              s"Number of match between source inParams and inEdge.uParam = " +
                s"${sourceModule.inParams.count(t => t.equals(inEdge.uParam))}"
            )

            // Construct DSALink
            accLinks += DSALink(
              sinkCompNode.nodeType,
              sinkCompNode.getNodeId,
              outIdx,
              sourceCompNode.nodeType,
              sourceCompNode.getNodeId,
              inIdx
            )
          }
        }
      }
    }
    // Return DSA Links for Computer System
    accLinks.result()
  }

  // Emit JSON for all connection between compute nodes, connections with sync nodes will be handle by SyncBuild
  def compEdges2JSON: List[JsObject] = {
    compDSALinks.map(x => x.toJSON)
  }

  // Emit JSON for nodes and edges
  def comp2JSON: JsObject =
    Json.obj(CompNodes.keyName -> JsObject(compNodes2JSON), CompEdges.keyName -> JsArray(compEdges2JSON))

  /* ---------- Bundle Collection ---------- */

  // Collect the config port wires of all compute nodes
  def getCompReconfNodes: Seq[ReconfNode] = compNodeModules.map(_.reconfNode.get)

  // Generate neighboring node mapping between compute nodes
  def compNeighborModules: Map[CompNodeModule, Seq[CompNodeModule]] = {
    // Get all of the compute node modules, without keep calling function
    val compModules: Seq[CompNodeModule] = compNodeModules
    // Build the neighboring node mapping based on existing edges connection
    val nearCompNodes: Map[CompNodeModule, Seq[CompNodeModule]] = {
      // Loop over all compute nodes
      compModules.map { currModule =>
        // Collect the source compute nodes if other modules' outward edges contain the inward edge of current modules
        val sourceModules: Seq[CompNodeModule] = currModule.node.edges.in.flatMap { inEdge =>
          compModules.filter(otherModule => otherModule.node.edges.out.contains(inEdge))
        }
        // Collect the sink compute nodes
        val sinkModules: Seq[CompNodeModule] = currModule.node.edges.out.flatMap { outEdge =>
          compModules.filter(otherModule => otherModule.node.edges.in.contains(outEdge))
        }
        // Build the mapping and distinct the neighboring nodes
        currModule -> (sourceModules ++ sinkModules).distinct
      }.toMap
    }
    // Return the neighboring modules mapping
    nearCompNodes
  }

  // Collect the busy signal from all compute nodes
  def getCompStatusPorts: Seq[CompNodeStatus] = compNodeModules.sortBy(_.nodeId).map(_.module.compStatus)
}
