package dsagen2.top.module

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.CompKeys.CompNode
import dsagen2.comp.module.CompNodeModule
import dsagen2.ctrl.config.WithDefaultStreamDispatcher
import dsagen2.ctrl.module.StreamDispatcherModule
import dsagen2.mem.config.MemKeys.MemNode
import dsagen2.mem.module.MemNodeModule
import dsagen2.sync.config.SyncKeys.{IVPNode, OVPNode}
import dsagen2.sync.module.{IVPNodeModule, OVPNodeModule}
import dsagen2.top.config.DSAFixedConfig.{CONF_FANOUT, MAX_RETRY_CONF}
import dsagen2.top.config.ParseKeys._
import dsagen2.top.config.{DSALink, DebugPrintable, PrintADGKey}
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.top.diplomacy.{DSANodeModule, ReconfNode}
import dsagen2.top.dsa.{CompBuild, MemBuild, SyncBuild}
import dsagen2.util.AppUtil.writeOutputFile
import dsagen2.util.CDE.json2cde
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile.{LazyRoCC, OpcodeSet, XLen}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** This is the Decoupled-Spatial Accelerator Building Environment
  * MemBuild:    Memory System Building Environment
  * SyncBuild:   Synchronization Building Environment
  * CompBuild:   Compute System Building Environment
  *
  * By default we have one PTW port, please add : override val nPTWPorts: Int = numDMA
  * at the end of your design if your have more than one DMA
  *
  * TODO: we will support more in future
  */
abstract class DSAGen(implicit p: Parameters)
    extends LazyRoCC(opcodes = OpcodeSet.all, nPTWPorts = 1)
    with MemBuild
    with SyncBuild
    with CompBuild
    with DebugPrintable {

  // Name of DSAGen
  implicit val valName: ValName = ValName(s"${this.getClass.getSimpleName}")
  val configTreeViz:    ListBuffer[String] = ListBuffer[String]()
  // Mapping to record the fromNode ==> list of nextNode
  val reconfNet: mutable.Map[ReconfNode, ListBuffer[ReconfNode]] = mutable.Map[ReconfNode, ListBuffer[ReconfNode]]()

  // Out-of-Order Stream Dispatcher
  // TODO: make it more reconfigurable in the future
  val dispatcher: StreamDispatcherModule = LazyModule(
    new StreamDispatcherModule(this)(p ++ new WithDefaultStreamDispatcher)
  )

  /* ------------------------------ Node Creation ------------------------------ */

  // Comp Node Creation
  def createCompNode(nodeConfig: Parameters)(implicit p: Parameters): CompNodeModule = {
    val nodeType: DSAGenNodeType = nodeConfig(CompNode).nodeType
    val nodeId:   Int = compNodeIdByNodeType(nodeType)
    // Update the node parameter with the existing nodeId
    val configNodeId: Parameters = nodeConfig.alterPartial({ case CompNode =>
      nodeConfig(CompNode).copy(nodeId = nodeId)
    })
    LazyModule(new CompNodeModule(nodeType, nodeId, numCOMP)(configNodeId ++ p, this))
  }

  // Memory Node Creation, TODO: not implemented yet
  def createMemNode(config: Parameters)(implicit p: Parameters): MemNodeModule = {
    // Get node Type
    val nodeType: DSAGenNodeType = config(MemNode).nodeType
    val nodeId:   Int = memNodeIdByNodeType(nodeType)
    // Update the node parameter with the existing nodeId and XLEN
    val configNodeId: Parameters = config.alterPartial({ case MemNode =>
      config(MemNode).copy(nodeId = nodeId, XLEN = p(XLen))
    })
    require(isMemNodeType(nodeType))
    // Modify DMA Width
    nodeType match {
      case DirectMemoryAccess =>
        // DMA's capacity should be same as system's
        val sysbusWidth: Int = p(SystemBusKey).beatBytes
        val newDMAConfig: Parameters = configNodeId.alterPartial({ case MemNode =>
          configNodeId(MemNode)
            .copy(readWidth = sysbusWidth, writeWidth = sysbusWidth, capacity = 1L << 40)
        })
        // Create the memory node
        LazyModule(new MemNodeModule(nodeType, nodeId)(newDMAConfig ++ p, this))
      case restNodeType: DSAGenNodeType =>
        // Create the memory node
        LazyModule(new MemNodeModule(restNodeType, nodeId)(configNodeId ++ p, this))
    }
  }

  // Create input vector port based on CDE parameter
  def createIVP(config: Parameters)(implicit p: Parameters): IVPNodeModule = {
    val nodeId: Int = syncNodeIdByNodeType(InputVectorPort)
    val configNodeId: Parameters = config.alterPartial({ case IVPNode =>
      config(IVPNode).copy(nodeId = nodeId)
    })
    LazyModule(new IVPNodeModule(nodeId = nodeId)(configNodeId ++ p, this))
  }

  // Create output vector port based on CDE
  def createOVP(config: Parameters)(implicit p: Parameters): OVPNodeModule = {
    val nodeId: Int = syncNodeIdByNodeType(OutputVectorPort)
    val configNodeId: Parameters = config.alterPartial({ case OVPNode =>
      config(OVPNode).copy(nodeId = nodeId)
    })
    LazyModule(new OVPNodeModule(nodeId = nodeId)(configNodeId ++ p, this))
  }

  /* ------------------------- Utility Function   ------------------------- */

  // Count the total number of node
  def numNode: Int = numCOMP + numMEM + numVP

  // Name as number
  def dsagenNumberName: ValName = ValName(
    s"${this.getClass.getSimpleName}." + compSysName + "." + memSysName + "." + syncSysName
  )

  // Filter the children and collect those children whose DSANodeType is contained in provided set
  def filterDSANode(nodeType: DSAGenNodeType*): Seq[DSANodeModule] = children.filter {
    case dsaModule: DSANodeModule => nodeType.contains(dsaModule.nodeType)
    case _ => false
  }.asInstanceOf[Seq[DSANodeModule]]

  // Print ADG File as Node Set and Edge Set
  def getADGJson: JsObject = Json.obj(
    DSAGenNodes.keyName -> JsObject(compNodes2JSON ++ memNodes2JSON ++ syncNodes2JSON),
    DSAGenEdges.keyName -> JsArray(topologicalSortLink(compDSALinks ++ memDSALinks ++ syncDSALinks).map(_.toJSON))
  )

  // Print Reconfiguration Node Connection
  def printConfConnection(sourceNode: ReconfNode, sinkNode: ReconfNode): Unit = {
    val str: String = "\"" + s"${sourceNode.dsaModule}" + "\" -> \"" + s"${sinkNode.dsaModule}" + "\";"
    if (printDebug) {
      configTreeViz += str
    }
  }

  // Record the reconfiguration net connection
  def recordConfConnection(sourceNode: ReconfNode, sinkNode: ReconfNode): Unit = {
    if (reconfNet.isDefinedAt(sourceNode)) {
      reconfNet(sourceNode) += sinkNode
    } else {
      reconfNet += sourceNode -> ListBuffer(sinkNode)
    }
  }

  // Limited Width DFS for building the reconfiguration network
  // Reason for limited width is that we do not want to create a large fanout in physics
  // Return
  // 1. A list of node and its depth in the reconfiguration tree
  // 2. Whether the network building is successful
  def buildNetLimitSearch(
    start:        ReconfNode,
    g:            ReconfNode => List[ReconfNode],
    visitedTrack: mutable.Map[ReconfNode, Boolean],
    width:        Int,
    doBFS:        Boolean = true
  ): (Map[ReconfNode, Int], Boolean) = {
    // Connect Reconfiguration Network by limited depth DFS
    def DFS0(
      v:          ReconfNode,
      visited:    List[(ReconfNode, Int)],
      limitWidth: Int,
      currDepth:  Int
    ): List[(ReconfNode, Int)] = {
      if (visited.contains((v, currDepth)))
        visited
      else {
        // Take first non-connected node as neighbours
        val neighbours: List[ReconfNode] = g(v).filterNot(x => visitedTrack(x)).take(limitWidth)
        // Connect the reconfiguration node
        neighbours.foreach { n =>
          printConfConnection(v, n)
          recordConfConnection(v, n) // n := v
          visitedTrack(n) = true
        }
        neighbours.foldLeft((v, currDepth) :: visited)((b, a) => DFS0(a, b, limitWidth, currDepth + 1))
      }
    }

    // Connect Reconfiguration Network by limited width DFS
    @tailrec
    def BFS0(elems: List[ReconfNode], visited: List[List[ReconfNode]]): List[List[ReconfNode]] = {
      // Mark visited for each node
      elems.foreach(e => visitedTrack(e) = true)
      // Collect all neighboring nodes
      val newNeighbors4Each: List[List[ReconfNode]] =
        elems.map(elem => scala.util.Random.shuffle(g(elem).filterNot(visited.flatten.contains)).take(width))
      val newNeighbors: List[ReconfNode] = newNeighbors4Each.flatten.distinct
      // Create the connected array for all nodes of next level
      val neighborConnected: mutable.Map[ReconfNode, Boolean] = mutable.Map(newNeighbors.map(x => x -> false): _*)
      // Connect the reconfiguration node
      require(newNeighbors4Each.length == elems.length)
      // Randomize the order of connection, do not aggregate connection for the header nodes
      scala.util.Random.shuffle(elems.zip(newNeighbors4Each)).foreach { case (v, ns) =>
        ns.foreach { n =>
          if (!neighborConnected(n)) {
            printConfConnection(v, n)
            recordConfConnection(v, n) // n := v
            neighborConnected(n) = true
          }
        }
      }
      // Sanity check, make sure all nodes from next level are connected
      require(neighborConnected.values.forall(x => x), s"all neighbors have to be connect")
      if (newNeighbors.isEmpty) visited else BFS0(newNeighbors, newNeighbors :: visited)
    }

    visitedTrack(start) = true
    if (doBFS) {
      var finish:            Boolean = false
      var visitNode2Depth:   Map[ReconfNode, Int] = null
      var noVisitNode2Depth: List[(ReconfNode, Int)] = null
      // Get the BFS result, traverse result for each layer
      val bfsResult = BFS0(List(start), List(List(start))).reverse
      // Collect all no-visited nodes
      val visitedNodes = bfsResult.flatten
      // Make suer all visited node is marked with true in visitedTrack
      require(visitedNodes.forall(n => visitedTrack(n)), s"Visited node are not all marked with true in track")
      val noVisitNodes = visitedTrack.filter(x => !x._2).keys.toList
      // Mark the depth for BFS result
      visitNode2Depth = bfsResult.zipWithIndex.flatMap { case (ns, depth) => ns.map(n => n -> depth) }.toMap
      // Calculate whether BFS is finished
      if (noVisitNodes.forall(noVisit => g(noVisit).exists(x => visitedNodes.contains(x)))) finish = true
      // For each not visited nodes, find the neighboring nodes with least number of outward edge
      noVisitNode2Depth = noVisitNodes.map { noVisit =>
        // make sure the node is not visited
        require(!visitedTrack(noVisit), s"Node $noVisit should not be visited")
        // Get neighboring visited nodes
        val neighborNodes = g(noVisit).filter(x => visitedNodes.contains(x))
        if (neighborNodes.nonEmpty) {
          // Get random one of above
          val randomNode = scala.util.Random.shuffle(neighborNodes).head
          // Random node should have depth
          require(visitNode2Depth.isDefinedAt(randomNode), s"Random selected node does not have depth")
          // Connect
          recordConfConnection(randomNode, noVisit) // noVisit := randomNode
          printConfConnection(randomNode, noVisit)
          // Add the depth
          noVisit -> (visitNode2Depth(randomNode) + 1)
        } else {
          noVisit -> -1
        }
      }
      ((visitNode2Depth.toList ++ noVisitNode2Depth).toMap, finish)
    } else {
      (DFS0(start, List(), width, 0).reverse.toMap, true)
    }
  }

  // Building Reconfiguration Network
  def buildConfigNetwork(initNode: ReconfNode, retry: Int = 0): Boolean = {
    // Collect neighboring node mapping, map it to be reconfiguration node for compute nodes
    val compReconfNeighborMap: mutable.Map[ReconfNode, Seq[ReconfNode]] =
      mutable.Map(compNeighborModules.map { case (module, modules) =>
        // All compute node should contains reconfiguration node
        require(module.reconfNode.isDefined)
        modules.foreach { m =>
          require(m.reconfNode.isDefined, s"Compute Node $m does not have reconfiguration node defined")
        }
        module.reconfNode.get -> modules.map(_.reconfNode.get)
      }.toSeq: _*)
    // Collect neighboring node mapping, map it to be reconfiguration node for input vector nodes
    val ivpReconfNeighborMap: mutable.Map[ReconfNode, Seq[ReconfNode]] = mutable.Map(ivpNeighborModules.filter {
      // Filter the input vector port node that has reconfiguration node defined first
      case (module, _) => module.reconfNode.isDefined
    }.map { case (module, modules) =>
      require(module.reconfNode.isDefined)
      modules.foreach { m =>
        require(
          m.reconfNode.isDefined,
          s"Node $m should be compute node that connects to ivp and " +
            s"should have reconfiguration node be defined, but it is not"
        )
      }
      module.reconfNode.get -> modules.map(_.reconfNode.get)
    }.toSeq: _*)
    // Collect neighboring node mapping, map it to be reconfiguration node for output vector nodes
    val ovpReconfNeighborMap: mutable.Map[ReconfNode, Seq[ReconfNode]] = mutable.Map(ovpNeighborModules.filter {
      // Filter the output vector port node that has reconfiguration node defined first
      case (module, _) => module.reconfNode.isDefined
    }.map { case (module, modules) =>
      require(module.reconfNode.isDefined)
      modules.foreach { m =>
        require(
          m.reconfNode.isDefined,
          s"Node $m should be compute node that connects to ovp and " +
            s"should have reconfiguration node be defined, but it is not"
        )
      }
      module.reconfNode.get -> modules.map(_.reconfNode.get)
    }.toSeq: _*)

    // Add COMP -> IVP/OVP to neighboring nodes of compute nodes
    ivpReconfNeighborMap.foreach { case (ivpCfgNode, compCfgNodes) =>
      compCfgNodes.foreach { compCfgNode =>
        if (!compReconfNeighborMap(compCfgNode).contains(ivpCfgNode)) {
          compReconfNeighborMap(compCfgNode) = compReconfNeighborMap(compCfgNode) ++ Seq(ivpCfgNode)
        }
      }
    }
    ovpReconfNeighborMap.foreach { case (ovpCfgNode, compCfgNodes) =>
      compCfgNodes.foreach { compCfgNode =>
        if (!compReconfNeighborMap(compCfgNode).contains(ovpCfgNode)) {
          compReconfNeighborMap(compCfgNode) = compReconfNeighborMap(compCfgNode) ++ Seq(ovpCfgNode)
        }
      }
    }

    // Merge all three neighboring reconfiguration nodes together
    val nodeGraph: mutable.Map[ReconfNode, List[ReconfNode]] =
      (compReconfNeighborMap ++ ivpReconfNeighborMap ++ ovpReconfNeighborMap).map(kv => kv._1 -> kv._2.toList)

    // Find the start node by selecting the node with less degree (to prevent wire aggregation)
    // TODO: We can definitely explore more option on picking the start node
    val pickIdx:   Int = retry / 100 % nodeGraph.size
    val startNode: ReconfNode = nodeGraph.toSeq.sortWith(_._2.length < _._2.length).apply(pickIdx)._1
    printConfConnection(initNode, startNode)
    recordConfConnection(initNode, startNode) // startNode := initNode

    // Initialize visited tracking
    val visitedTrack: mutable.Map[ReconfNode, Boolean] = mutable.Map(nodeGraph.keys.toSeq.map(n => n -> false): _*)

    // Connect all reconfiguration nodes by applying limited width DFS
    val (visitedNode2Depth, success): (Map[ReconfNode, Int], Boolean) =
      buildNetLimitSearch(startNode, nodeGraph, visitedTrack, CONF_FANOUT)

    // For graph that may have multiple start point, connect them with initNode
    nodeGraph.keys.foreach { v =>
      if (!visitedNode2Depth.isDefinedAt(v)) {
        printConfConnection(initNode, v)
        recordConfConnection(initNode, v) // v := initNode
      }
    }

    // Make sure all nodes has exactly one input reconfiguration edge
    val exactOneReconfIn: Boolean = nodeGraph.keys.forall { v => reconfNet.count(x => x._2.result().contains(v)) == 1 }

    // Make sure all reconfiguration nodes has at least one input
    val maxFanout: Int = reconfNet.values.map { v => v.length }.max

    // Get the depth of reconfiguration network
    reconfDepth = visitedNode2Depth.values.max
    if (printDebug) {
      val str: String = s"Max Fanout = $maxFanout"; configTreeViz += s"// $str"
    }
    if (printDebug) {
      val str: String = s"Tree Depth = $reconfDepth"; configTreeViz += s"// $str"
    }

    // Return the success of network building
    success && exactOneReconfIn && maxFanout <= CONF_FANOUT
  }

  // Connect reconfiguration port of all compute nodes
  def autoConfigure(): Unit = {
    // Building Reconfiguration Network At the beginning of DSAGen Implementation
    if (dispatcher.ctrlParam.reconfNetwork) {
      // Initialize the looping variable
      var success: Boolean = false
      var retry:   Int = 1
      // Building the network
      while (!success) {
        configTreeViz.clear()
        reconfNet.clear()
        success = buildConfigNetwork(dispatcher.initReconfNode)
        if (printDebug) configTreeViz += "}"
        // Timeout
        retry += 1
        require(retry < MAX_RETRY_CONF, s"Reconfiguration network build fail after $MAX_RETRY_CONF times retry")
      }
      // Print the final reconfiguration network
      if (printDebug) {
        println(s"---------- Reconfiguration Network Build Start: TRY $retry ----------")
        configTreeViz.dropRight(3).foreach(println)
        println(s"---------- Reconfiguration Network Build  End : TRY $retry ----------")
      }
      // Connect the reconfigurable node
      reconfNet.foreach { case (currNode, nextNodes) => nextNodes.foreach(nxtNode => nxtNode := currNode) }
    } else {
      // Connect config ports of the compute system from stream dispatcher by using BUS, very high fanout!
      this.getCompReconfNodes.foreach { compReconfNode =>
        // Connect to each one of them
        compReconfNode := dispatcher.initReconfNode
      }
      reconfDepth = 1
    }
  }

  // Write DSA's Architecture Description Graph (ADG) to directory specified by DSAGEN_DIR or ADG_DIR
  def writeADG(): Unit = {
    p.lift(PrintADGKey) match {
      case Some((dir, fname)) =>
        import java.time.format.DateTimeFormatter
        val newTS: String = DateTimeFormatter.ofPattern(".yyyyMMdd-HHmmss").format(LocalDateTime.now)
        // generate name if the given one is empty
        val adgName = if (fname == "") dsagenNumberName.name + newTS + ".json" else fname
        val dotName = if (fname == "") dsagenNumberName.name + newTS + ".dot" else fname
        // Print
        if (printDebug)
          println(
            s"Architecture Description Graph ==> \n " +
              s"$dir/$adgName"
          )
        // Write Architecture Description Graph
        writeOutputFile(dir, adgName, Json.prettyPrint(getADGJson))
        writeOutputFile(
          dir,
          dotName,
          configTreeViz
            .result()
            .foldLeft("digraph configTree {")((prevStr, curr) => prevStr + "\n" + curr)
        )
      case None =>
      // Do not print ADG
    }
  }

  // Since the index hint the connection order we have to sort it
  def topologicalSortLink(unsortLinks: Seq[DSALink]): Seq[DSALink] = {
    val resultBuffer: ListBuffer[DSALink] = ListBuffer[DSALink]()

    import scalax.collection.Graph
    import scalax.collection.GraphPredef.EdgeAssoc

    // Find the Partial Order
    val partialOrder = unsortLinks.flatMap { link =>
      val successorLink = unsortLinks.filter { candidate =>
        val sourceSuccessor: Boolean =
          candidate.sourceNodeType == link.sourceNodeType &&
            candidate.sourceNodeId == link.sourceNodeId &&
            candidate.sourceEdgeIdx > link.sourceEdgeIdx
        val sinkSuccessor: Boolean =
          candidate.sinkNodeType == link.sinkNodeType &&
            candidate.sinkNodeId == link.sinkNodeId &&
            candidate.sinkEdgeIdx > link.sinkEdgeIdx
        sourceSuccessor || sinkSuccessor
      }
      successorLink.map(succ => link ~> succ)
    }

    // Build the graph
    val dag = Graph(partialOrder: _*)

    // Topological Sort
    dag.topologicalSort.fold(
      // Detect the loop dependency
      cycleNode =>
        require(
          requirement = false,
          s"There is a cycle node in graph : $cycleNode;" +
            s"Did you make connections like node.1[1] --> node.2[0], node.1[0] --> node.2[1]?" +
            s"This is diplomacy problem, since it use the creation order of topology as port index"
        ),
      // Order found, enqueue to buffer
      order => order.toLayered.foreach(layer => layer._2.foreach(x => resultBuffer += x))
    )

    if (printDebug) {
      println(s"Links connection with topological order : ")
      println(s"------------------------------------------")
      resultBuffer.result().foreach(println)
      println(s"------------------------------------------")
    }
    resultBuffer.result()
  }

  // DSAGen module implementation
  lazy val module = new DSAGenImp(this)
}

object DSAGen {

  /** For an given node described by JSON object and an potential node ID,
    * check weather this node exists
    *
    * @param adgNodes JSON Object that contains all DSAGen Nodes
    * @param nodeType Node Type of the potential check
    * @param nodeId   Node Id that will be checked
    * @return Whether such node exists
    */
  def nodeIdExist(adgNodes: JsObject, nodeType: DSAGenNodeType, nodeId: Int): Boolean = {
    val keyName: String = nodeType.toString + "." + nodeId.toString
    val exist:   Boolean = adgNodes.value.isDefinedAt(keyName)
    val nodeParam = adgNodes.value(keyName)
    // Get the internal node type id in the Node definition by node Type
    // (since key class is different)
    var inNodeTypeExist: Boolean = false
    var inNodeType:      String = ""
    var inNodeIdExist:   Boolean = false
    var inNodeId:        Int = -1
    // Collect the internal
    nodeType match {
      case ProcessingElement =>
        val nodeIdLookup = nodeParam \ CompNode.getClass.getName \ "nodeId"
        val nodeTypeLookup = nodeParam \ CompNode.getClass.getName \ "nodeType"
        inNodeTypeExist = nodeTypeLookup.isDefined
        inNodeType = nodeTypeLookup.as[String]
        inNodeIdExist = nodeIdLookup.isDefined
        inNodeId = nodeIdLookup.as[Int]
      case Switch =>
        val nodeIdLookup = nodeParam \ CompNode.getClass.getName \ "nodeId"
        val nodeTypeLookup = nodeParam \ CompNode.getClass.getName \ "nodeType"
        inNodeTypeExist = nodeTypeLookup.isDefined
        inNodeType = nodeTypeLookup.as[String]
        inNodeIdExist = nodeIdLookup.isDefined
        inNodeId = nodeIdLookup.as[Int]
      case InputVectorPort =>
        val nodeIdLookup = nodeParam \ IVPNode.getClass.getName \ "nodeId"
        val nodeTypeLookup = nodeParam \ IVPNode.getClass.getName \ "nodeType"
        inNodeTypeExist = nodeTypeLookup.isDefined
        inNodeType = nodeTypeLookup.as[String]
        inNodeIdExist = nodeIdLookup.isDefined
        inNodeId = nodeIdLookup.as[Int]
      case OutputVectorPort =>
        val nodeIdLookup = nodeParam \ OVPNode.getClass.getName \ "nodeId"
        val nodeTypeLookup = nodeParam \ OVPNode.getClass.getName \ "nodeType"
        inNodeTypeExist = nodeTypeLookup.isDefined
        inNodeType = nodeTypeLookup.as[String]
        inNodeIdExist = nodeIdLookup.isDefined
        inNodeId = nodeIdLookup.as[Int]
      case memNodeType: DSAGenNodeType =>
        val nodeIdLookup = nodeParam \ MemNode.getClass.getName \ "nodeId"
        val nodeTypeLookup = nodeParam \ MemNode.getClass.getName \ "nodeType"
        inNodeTypeExist = nodeTypeLookup.isDefined
        inNodeType = nodeTypeLookup.as[String]
        inNodeIdExist = nodeIdLookup.isDefined
        inNodeId = nodeIdLookup.as[Int]
    }

    require(
      exist && inNodeIdExist && inNodeTypeExist &&
        (nodeId == inNodeId) && (nodeType == String2DSANodeType(inNodeType)),
      s"Look for DSAGen Node $nodeType $nodeId, lookup result = " +
        s"Node existence : $exist" +
        s"nodeType exist : $inNodeTypeExist, nodeType : $inNodeType ;" +
        s"nodeId exist : $inNodeIdExist, nodeId : $inNodeId"
    )

    exist && inNodeIdExist && inNodeTypeExist &&
    (nodeId == inNodeId) && (nodeType == String2DSANodeType(inNodeType))
  }

  /** Perform sanity check for the JSON object for the compute system:
    *  1. Check whether this JSON contains `Nodes` and `Edges`
    *     2. For the source node and sink node of each edge,
    *     make sure that it exists in Nodes definition
    *
    * @param json The JSON object of total compute system
    */
  def jsonSanityCheck(json: JsValue): Unit = {
    // Must have Nodes and Edges
    require(
      (json \ DSAGenNodes.keyName).isDefined && (json \ DSAGenEdges.keyName).isDefined,
      s"DSAGen ADG Json must have ${DSAGenNodes.keyName} and ${DSAGenEdges.keyName} keys"
    )
    // All edges' source nodeId and sink nodeId should be defined in node
    val nodes: JsObject = (json \ DSAGenNodes.keyName).get.as[JsObject]
    val edges: JsArray = (json \ DSAGenEdges.keyName).get.as[JsArray]
    edges.value.foreach { jsVal =>
      val edgePairJson = jsVal.as[JsObject]
      val sourceNodeType: DSAGenNodeType =
        String2DSANodeType((edgePairJson \ DSALinkSourceNodeType.keyName).as[String])
      val sinkNodeType: DSAGenNodeType =
        String2DSANodeType((edgePairJson \ DSALinkSinkNodeType.keyName).as[String])
      val sourceNodeId: Int = (edgePairJson \ DSALinkSourceNodeId.keyName).as[Int]
      val sinkNodeId:   Int = (edgePairJson \ DSALinkSinkNodeId.keyName).as[Int]
      require(
        nodeIdExist(nodes, sourceNodeType, sourceNodeId),
        s"Edge points from node($sourceNodeType $sourceNodeId) but it does not exist"
      )
      require(
        nodeIdExist(nodes, sinkNodeType, sinkNodeId),
        s"Edge points to node($sinkNodeType $sinkNodeId) but it does not exist"
      )
    }
  }

  /** Convert a array of DSALink object to sequence of DSALink
    *
    * @param jsonEdges Json array
    * @return Sequence of parsed DSALink
    */
  def jsonEdges2DSALinks(jsonEdges: JsArray): Seq[DSALink] = {
    // Convert it to DSALink
    val unsorted: Seq[DSALink] = jsonEdges.value.map {
      case JsObject(edge) =>
        DSALink(
          sourceNodeType = edge(DSALinkSourceNodeType.keyName).as[String],
          sourceNodeId = edge(DSALinkSourceNodeId.keyName).as[Int],
          sourceEdgeIdx = edge(DSALinkSourceIndex.keyName).as[Int],
          sinkNodeType = edge(DSALinkSinkNodeType.keyName).as[String],
          sinkNodeId = edge(DSALinkSinkNodeId.keyName).as[Int],
          sinkEdgeIdx = edge(DSALinkSinkIndex.keyName).as[Int]
        )
      case _ =>
        require(requirement = false, s"DSA Edges should be a json array of DSALink object")
        DSALink(Switch, -1, -1, Switch, -1, -1)
    }
    // Sort the sequence for ordered connection
    unsorted
  }

  /** DSAGen convert the parsed JSON object to DSAGen architecture module implementatino
    *
    * @param adgJson Architecture Description Graph in JSON File Format
    * @return
    */
  def apply(adgJson: JsValue)(implicit p: Parameters): DSAGen = new DSAGen()(p) {
    // Sanity Check Parsed Json
    jsonSanityCheck(adgJson)

    // Collect JSON object for compute node and edges
    val dsaNodes: JsObject = (adgJson \ DSAGenNodes.keyName).get.as[JsObject]

    // Parse the edges as DSALink
    val dsaLinks: Seq[DSALink] =
      jsonEdges2DSALinks((adgJson \ DSAGenEdges.keyName).get.as[JsArray])

    // Instantiate DSAGen Node and create a (nodeType, nodeId) to node module mapping
    val nodeName2CompNode: Map[(DSAGenNodeType, Int), LazyModule] =
      dsaNodes.fields.map {
        case (nodeName, jsonObj) =>
          val (nodeType, nodeId) = nodeName2pair(nodeName)
          val config: Parameters = json2cde(jsonObj)
          val genModule = nodeType match {
            case ProcessingElement => createCompNode(config)
            case Switch            => createCompNode(config)
            case InputVectorPort   => createIVP(config)
            case OutputVectorPort  => createOVP(config)
            case memNodeType: DSAGenNodeType =>
              isMemNodeType(memNodeType)
              createMemNode(config)
          }
          (nodeType, nodeId) -> genModule
        case _ =>
          require(requirement = false, "CDE parameters can only be parsed as Json Object")
          val dummy = createCompNode(Parameters.empty)(Parameters.empty)
          (Switch, 114514) -> dummy
      }.toMap

    // Topological Sort to find the correct order to connect
    val dsaLinksOrder: Seq[DSALink] = topologicalSortLink(dsaLinks)

    // Instantiate Connection
    for (dsaLink <- dsaLinksOrder) {
      // It is the time to connect it
      if (isCompNodeType(dsaLink.sourceNodeType) && isCompNodeType(dsaLink.sinkNodeType)) {
        // Connection among Compute Node
        val sourceNodeModule: CompNodeModule =
          nodeName2CompNode((dsaLink.sourceNodeType, dsaLink.sourceNodeId)).asInstanceOf[CompNodeModule]
        val sinkNodeModule: CompNodeModule =
          nodeName2CompNode((dsaLink.sinkNodeType, dsaLink.sinkNodeId)).asInstanceOf[CompNodeModule]
        sourceNodeModule --> sinkNodeModule
      } else if (dsaLink.sourceNodeType == InputVectorPort && isCompNodeType(dsaLink.sinkNodeType)) {
        // Connection between IVP to Comp Node Module
        val sourceNodeModule: IVPNodeModule =
          nodeName2CompNode((dsaLink.sourceNodeType, dsaLink.sourceNodeId)).asInstanceOf[IVPNodeModule]
        val sinkNodeModule: CompNodeModule =
          nodeName2CompNode((dsaLink.sinkNodeType, dsaLink.sinkNodeId)).asInstanceOf[CompNodeModule]
        sourceNodeModule --> sinkNodeModule
      } else if (dsaLink.sinkNodeType == OutputVectorPort && isCompNodeType(dsaLink.sourceNodeType)) {
        // Connection between Comp to OVP Node Module
        val sourceNodeModule: CompNodeModule =
          nodeName2CompNode((dsaLink.sourceNodeType, dsaLink.sourceNodeId)).asInstanceOf[CompNodeModule]
        val sinkNodeModule: OVPNodeModule =
          nodeName2CompNode((dsaLink.sinkNodeType, dsaLink.sinkNodeId)).asInstanceOf[OVPNodeModule]
        sourceNodeModule --> sinkNodeModule
      } else if (isMemNodeType(dsaLink.sourceNodeType)) {
        // Connection between Memory node to IVP Node Module
        val sourceNodeModule: MemNodeModule =
          nodeName2CompNode((dsaLink.sourceNodeType, dsaLink.sourceNodeId)).asInstanceOf[MemNodeModule]
        val sinkNodeModule: IVPNodeModule =
          nodeName2CompNode((dsaLink.sinkNodeType, dsaLink.sinkNodeId)).asInstanceOf[IVPNodeModule]
        sourceNodeModule --> sinkNodeModule
      } else {
        require(isMemNodeType(dsaLink.sinkNodeType))
        val sourceNodeModule: OVPNodeModule =
          nodeName2CompNode((dsaLink.sourceNodeType, dsaLink.sourceNodeId)).asInstanceOf[OVPNodeModule]
        val sinkNodeModule: MemNodeModule =
          nodeName2CompNode((dsaLink.sinkNodeType, dsaLink.sinkNodeId)).asInstanceOf[MemNodeModule]
        sourceNodeModule --> sinkNodeModule
      }
      println(
        s"${dsaLink.sourceNodeType}.${dsaLink.sourceNodeId}[${dsaLink.sourceEdgeIdx}]" +
          s" --> ${dsaLink.sinkNodeType}.${dsaLink.sinkNodeId}[${dsaLink.sinkEdgeIdx}]"
      )
    }

    autoConfigure()
    tlNode := getMemAggTLNode
    override val nPTWPorts: Int = numDMA
  }
}
