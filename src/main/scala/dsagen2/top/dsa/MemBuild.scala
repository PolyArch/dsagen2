package dsagen2.top.dsa

import chisel3._
import chisel3.util.{DecoupledIO, log2Ceil}
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle.MemoryNodeStatus
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.mem.module.MemNodeModule
import dsagen2.sync.module.{IVPNodeModule, OVPNodeModule}
import dsagen2.top.config.DSALink
import dsagen2.top.config.ParseKeys.{MemEdges, MemNodes}
import dsagen2.top.config.operation.Operation.DsaOperation
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, DirectMemoryAccess, DiscardEngine, GenerateEngine, RecurrenceEngine, RegisterEngine, ScratchpadMemory}
import dsagen2.top.module.DSAGen
import dsagen2.util.RegUtil.RegNextN
import freechips.rocketchip.rocket.TLBPTWIO
import freechips.rocketchip.tile.{RoCCResponse, XLen}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLNode, TLXbar}
import play.api.libs.json.{JsArray, JsObject, Json}

import scala.collection.mutable.ListBuffer

trait MemBuild {
  dsagen: DSAGen =>

  /* ------------------------------ Node Collection ------------------------------ */

  // Collect Memory Nodes of certain type
  private def memModules(memNodeTypes: DSAGenNodeType*): Seq[MemNodeModule] =
    filterDSANode(memNodeTypes: _*).asInstanceOf[Seq[MemNodeModule]]

  // Collect All Memory Modules, MemBuild only has MemNodeModule
  private def allMemModules: Seq[MemNodeModule] = {
    memModules(DirectMemoryAccess, ScratchpadMemory, RecurrenceEngine, DiscardEngine, GenerateEngine, RegisterEngine)
      .sortBy(_.nodeId)
  }

  /* ------------------------------ Statistic Collection ------------------------------ */

  // Generate name
  def memSysName: String = {
    s"${if (numDMA > 0) s"DMA$numDMA" else ""}" +
      s"${if (numSPM > 0) s".SPM$numSPM" else ""}" +
      s"${if (numREC > 0) s".REC$numREC" else ""}" +
      s"${if (numDIS > 0) s".DIS$numDIS" else ""}" +
      s"${if (numGEN > 0) s".GEN$numGEN" else ""}" +
      s"${if (numREG > 0) s".REG$numREG" else ""}"
  }

  // Count different kinds of memory node
  def numDMA: Int = memModules(DirectMemoryAccess).length

  def numSPM: Int = memModules(ScratchpadMemory).length

  def numREC: Int = memModules(RecurrenceEngine).length

  def numDIS: Int = memModules(DiscardEngine).length

  def numGEN: Int = memModules(GenerateEngine).length

  def numREG: Int = memModules(RegisterEngine).length

  def numMEM: Int = {
    val numTotal = numDMA + numSPM + numREC + numDIS + numGEN + numREG
    require(numTotal == allMemModules.length)
    numTotal
  }

  // Get Node Id per node Type
  def memNodeIdByNodeType(memNodeType: DSAGenNodeType): Int = {
    memNodeType match {
      case DirectMemoryAccess => numDMA
      case ScratchpadMemory   => numSPM
      case RecurrenceEngine   => numREC
      case DiscardEngine      => numDIS
      case GenerateEngine     => numGEN
      case RegisterEngine     => numREG
      case errType: DSAGenNodeType =>
        require(requirement = false, s"Node Type $errType cannot be supported by Mem"); -1
      case _ => require(requirement = false, s"You are not providing nodeType"); -1
    }
  }

  // Collect all memory node parameters from memory modules
  def allMemNodeParam: Seq[MemNodeParameters] = allMemModules.map(_.memNode)

  // Collect the max data type supported across all memory nodes
  def maxMemDataType: Int = allMemNodeParam.map(_.supportMemDataTypes(p(XLen)).max).max

  // Collect the minimum memory unit bits across all memory node
  def minMemUnitBits: Int = {
    val allMemUnitBits: Seq[Int] = allMemNodeParam.map(_.memUnitBits)
    // TODO: for now, we do not allow different memory base unit bits (usually byte)
    require(allMemUnitBits.length == 1, "for now, we do not allow different memory base unit bits (usually byte)")
    require(allMemUnitBits.head == 8, s"Memory should be byte addressable, but it is ${allMemUnitBits.head}")
    allMemUnitBits.head
  }

  // Max number of bit to encode the memory stream data type
  def maxMemDataTypeBits: Int = allMemNodeParam.map(_.memDataTypeBits).max

  // Calculate the number of bit needed to encode constant stream data type
  def maxConstDataTypeBits: Int = memModules(GenerateEngine) match {
    // If there is no generate engine in memory system, 0 bit for this field
    case Nil => 0
    case generateEngines: Seq[MemNodeModule] => generateEngines.map(_.memNode.constDataTypeBits).max
  }

  // Get the maximum number of read port
  def maxNumRead: Int = {
    val max: Int = allMemNodeParam.map(_.numRead).max
    // There must be at least one read port, otherwise how you send the data to compute system
    require(max >= 1, s"At least one read port is required across all memory node, but it is $max now")
    max
  }

  // Get the maximum number of write port
  def maxNumWrite: Int = {
    val max: Int = allMemNodeParam.map(_.numWrite).max
    require(max >= 1, s"At least one write port is required across all memory node, but it is $max now")
    max
  }

  // Calculate the union of atomic operation set and assign encoding
  def fullAtomicOpEnc: Map[DsaOperation, Int] = {
    // collect atomic operation from all memory node
    val fullAops: Seq[DsaOperation] =
      allMemNodeParam.map(_.AtomicOperations).reduce(_ union _).toSeq.sortWith(_.toString < _.toString)
    // for now we only support up to 6 kinds of atomic operations in memory system
    require(fullAops.length <= 6, s"For now we support up to 6 atomic operation, but it is ${fullAops.length}")
    // Assign encoding to each atomic operation, starting from 2, 0 is read, 1 is write
    fullAops match {
      case Nil => Map.empty
      case seq: Seq[DsaOperation] =>
        (for (idx <- fullAops.indices) yield {
          seq(idx) -> (idx + 2) // starting with two
        }).toMap
    }
  }

  // Calculate the number of bit needed for encoding the memory operation (read/write/atomic operation)
  def memOpBits: Int = log2Ceil(2 + fullAtomicOpEnc.size)

  // Calculate the max number of bit needed for encoding the stride 1D
  def maxStride1DBits: Int = allMemNodeParam.map(_.stride1DBits).max

  // Calculate the max number of bit needed for encoding the stride 2D
  def maxStride2DBits: Int = allMemNodeParam.map(_.stride2DBits).max

  // Calculate the max number of bit needed for encoding the stride 3D
  def maxStride3DBits: Int = allMemNodeParam.map(_.stride3DBits).max

  // Collect the max number of linear dimension
  def maxNumLinearDim: Int = allMemNodeParam.map(_.numLinearDimension).max

  // Check if any memory node support linear padding mode
  def supportLinearPadding: Boolean = allMemNodeParam.exists(_.LinearPadding)

  // Collect the max number of bit to encode Length 1D field
  def maxLength1DBits: Int = allMemNodeParam.map(_.length1DBits).max

  // Collect the max number of bit to encode Length 2D field
  def maxLength2DBits: Int = allMemNodeParam.map(_.length2DBits).max

  // Collect the max number of bit to encode Length 3D field
  def maxLength3DBits: Int = allMemNodeParam.map(_.length3DBits).max

  // Collect the max number of bit to encode stretch 2D field
  def maxStretch2DBits: Int = allMemNodeParam.map(_.stretch2DBits).max

  // Collect the max number of bit to encode stretch 3D to 2D field
  def maxStretch3D2DBits: Int = allMemNodeParam.map(_.stretch3D2DBits).max

  // Collect the max number of bit to encode stretch 3D to 1D field
  def maxStretch3D1DBits: Int = allMemNodeParam.map(_.stretch3D1DBits).max

  // Collect the max number of bit to encode delta Stride 2D field
  def maxDeltaStride2DBits: Int = allMemNodeParam.map(_.deltaStride2DBits).max

  // Collect the max number of bit to encode delta stretch 2D field
  def maxDeltaStretch2DBits: Int = allMemNodeParam.map(_.deltaStretch2DBits).max

  // Check whether or not there is memory support linear pattern
  def supportLinear: Boolean = allMemNodeParam.exists(_.supportLinear)

  // Check whether or not there is memory support indirect pattern
  def supportIndirect: Boolean = allMemNodeParam.exists(_.supportIndirect)

  // Check whether or not support indirect index stream
  def supportIndirectIdx: Boolean = allMemNodeParam.exists(_.IndirectIndexStream)

  // Check whether or not support dual mode stride 2D stream
  def supportDualModeStride2D: Boolean = allMemNodeParam.exists(_.dualModeStride2D)

  // Check whether or not support dual mode length 1D stream
  def supportDualModeLength1D: Boolean = allMemNodeParam.exists(_.dualModeLength1D)

  // Collect the max number of indirect dimension
  def maxNumIndirectDim: Int = allMemNodeParam.map(_.numIndirectDimension).max

  // Check whether support indirect stride 2D stream across all memory nodes
  def supportIndirectS2D: Boolean = allMemNodeParam.exists(_.IndirectStride2DStream)

  // Check whether support indirect length 1D stream across all memory nodes
  def supportIndirectL1D: Boolean = allMemNodeParam.exists(_.IndirectLength1DStream)

  // Collect the max number of bit needed by specifying index stream data type
  def maxIdxStrDataTypeBits: Int = allMemNodeParam.map(_.idxStrDataTypeBits).max

  // Collect the max number of bit needed by specifying stride 2D stream data type
  def maxStride2DStrDataTypeBits: Int = allMemNodeParam.map(_.s2dStrDataTypeBits).max

  // Collect the max number of bit needed by specifying length 1D stream data type
  def maxLength1DStrDataTypeBits: Int = allMemNodeParam.map(_.l1dStrDataTypeBits).max

  // Whether the memory system support linear and indirect pattern at the same time
  def supportLinearAndIndirect: Boolean = supportLinear && supportIndirect

  // Calculate the total bit of all data type exponential
  def totalDataTypeBits: Int =
    maxMemDataTypeBits + maxConstDataTypeBits + maxIdxStrDataTypeBits +
      maxStride2DStrDataTypeBits + maxLength1DStrDataTypeBits

  // Check whether this is a dsa with scratchpad memory that supports buffet
  def supportBuffet: Boolean = allMemNodeParam.exists(_.supportBuffet)

  /* ------------------------------ Bundle Collection ------------------------------ */

  // Create stream dispatcher port as input and connect it to each of the memory nodes
  // Stream Dispatch, Mandate wire, size = 1, there is no need to have more than one dispatch window, since
  // instruction is forwarded from RoCC, which can only be at most one per cycle.
  def getStrDispBus: StreamDispatchBus = {
    // create the stream dispatch bus
    val sdp: StreamDispatchBus = Wire(new StreamDispatchBus(dsagen))
    // Stream Dispatch to all Memory Modules
    allMemModules.foreach { m =>
      // Connect Stream Dispatch Port to each of the memory modules
      //
      m.module.strDisp := RegNextN(sdp, dispatcher.ctrlParam.dispStage - 1).last
      // This is where we dispatch to the correct place
      m.module.strDisp.valid := RegNextN(
        sdp.valid && sdp.memType === m.memNode.hwMemType,
        dispatcher.ctrlParam.dispStage - 1
      ).last
    }
    // return
    sdp
  }

  // Create RoCC response wire for memory system with Register Engine
  // Optional wire, only for those memory system with register node
  def getRegResp: Option[DecoupledIO[RoCCResponse]] = {
    if (dsagen.numREG > 0) {
      // Create RoCC response
      val rr: Option[DecoupledIO[RoCCResponse]] = Some(Wire(DecoupledIO(new RoCCResponse)))
      // Collect all register node and arbitrate between them to RoCC response
      val rdRespPorts: Seq[DecoupledIO[RoCCResponse]] = memModules(RegisterEngine).map(_.module.rdResp.get)
      (rr, rdRespPorts) match {
        case (None, Nil) => // No register engine, then do nothing
        case (Some(aggRdRespPort), headRdResp :: tailRdResps) => // arbitrate them with arbiter
          val rdRespPorts: Seq[DecoupledIO[RoCCResponse]] = headRdResp :: tailRdResps
          // Since register entry is selected in Robin-Round, so RRArbiter is not required
          if (rdRespPorts.length == 1) aggRdRespPort <> rdRespPorts.head
          else {
            for (rdRespPort <- rdRespPorts) {
              // There is no arbitration since stream table will make sure only one entry is valid at a time
              when(rdRespPort.valid) {
                aggRdRespPort.valid := true.B
                aggRdRespPort.bits.rd := rdRespPort.bits.rd
                aggRdRespPort.bits.data := rdRespPort.bits.data
              }
              // Ready is broadcast to all register engine
              rdRespPort.ready := aggRdRespPort.ready
            }
          }
        case _ => require(requirement = false, s"Register Response Port is problematic")
      }
      // return
      rr
    } else {
      None
    }
  }

  // Create Page Table Walker wire, only for memory system with DMAs, vector length = #DMA
  def getPTWs: Option[Vec[TLBPTWIO]] = {
    if (memModules(DirectMemoryAccess).nonEmpty) {
      // create the page table walker wires for each DMA node
      val ptws: Option[Vec[TLBPTWIO]] = Some(Wire(Vec(dsagen.numDMA, new TLBPTWIO)))
      // connect page table walker to each DMA node
      // Page Table Walker, only connect the Page Table Walker IO to DMA
      ptws match {
        case Some(ptws) =>
          ptws.zip(memModules(DirectMemoryAccess)).foreach { case (tlbptwio, module) =>
            tlbptwio <> module.module.ptw.get
          }
        case None => // do nothing
      }
      // return
      ptws
    } else None
  }

  // Create Memory Node Status wire, mandate, for each memory node
  def getMemStatus: Vec[MemoryNodeStatus] = {
    // create bundle
    val memStatus: Vec[MemoryNodeStatus] =
      WireInit(VecInit(Seq.fill(numMEM)(0.U.asTypeOf(new MemoryNodeStatus))))
    // collect memory node status from all memory node
    // Memory Node Status, connect the memory node status back
    memStatus.zip(allMemModules).foreach { case (status, module) =>
      status := module.module.memStatus
    }
    // return
    memStatus
  }

  // Collect the config ports from the first DMA node
  // TODO: we should move the bitstream loading to DMA and SPM in the future
  /*  def getMemConfigPorts : MixedVec[UInt] = {
    // Sanity check: only one and must have one DMA has the configuration bitstream mapping
    require(memModules(DirectMemoryAccess).count(dma => dma.configBitsMap.nonEmpty) == 1,
      s"The number of DMA engine that has configuration bitstream mapping needs to be equal to 1")
    // Select the first DMA to be responsible for configuration bitstream loading
    memModules(DirectMemoryAccess) match {
      // Get the first DMA TODO: I am not very sure whether this will give us the first DMA created
      case firstDMA :: _ =>
        // Make sure that the first DMA's configuration mapping is not set yet
        require(firstDMA.configBitsMap.nonEmpty, s"The first DMA's config port bits has not been set")
        // Connect the first DMA's configPorts to memory system's configPorts
        firstDMA.module.configPorts match {
          // Return the config ports of that DMA node
          case Some(cp) => cp
          case None =>
            require(requirement = false, "The first DMA's configuration port is None")
            Wire(MixedVec(Seq(0.U)))
        }
      case Nil =>
        require(requirement = false, s"Memory System must have at least one DMA engine to support bitstream loading")
        Wire(MixedVec(Seq(0.U)))
    }
  }*/

  // Collect the TileLink Node for all memory nodes
  def getMemAggTLNode: TLIdentityNode = {
    require(memModules(DirectMemoryAccess).nonEmpty, s"There is no DMA in this memory system")

    // The single TLNode of memory system for all DMAs with one XBar
    val memAggNode: TLIdentityNode = TLIdentityNode()

    // For N DMAs, we have 1 memAggNode <- 1 aggXbar <- N dmaXbar <- N x (reader, writer)
    val aggXbar: TLNode = TLXbar()

    // Connect the XBar to the ID node of memory system
    memAggNode := aggXbar

    // Connect reader and writer from all DMAs to the tilelink crossbar
    memModules(DirectMemoryAccess).foreach { dma =>
      // Connect DMA local tile node to crossbar
      dma.tlNode match {
        case Some(dmaTLNode) => aggXbar := dmaTLNode
        case None =>
          require(requirement = false, "Your DMA does not contain TileLink Node, it cannot access System Bus")
      }
    }

    // Return the memory system TileLink aggregation node
    memAggNode
  }

  // Sanity Check: memory system need to support at least one stream pattern: linear or indirect
  // require(supportLinear || supportIndirect, s"You have to support at least one memory pattern")

  // Emit JSON for Memory Nodes
  def memNodes2JSON: Seq[(String, JsObject)] =
    allMemModules.map { module => s"${module.memNode.getNodeName}" -> module.toJSON }

  // Get DSALink for Memory <-> IVP/OVP
  def memDSALinks: List[DSALink] = {
    // The buffer the hold all links
    val accLinks: ListBuffer[DSALink] = ListBuffer[DSALink]()
    // Loop over all memory nodes
    for (memModule <- allMemModules) {
      // Get all edges from memory to ivp, from ovp to memory
      val outEdges: Seq[Mem2IVPParameter] = memModule.readNode.out.map(_._2)
      val inEdges:  Seq[OVP2MemParameter] = memModule.writeNode.in.map(_._2)
      // Loop over all output edges
      for ((outEdge, outIdx) <- outEdges.zipWithIndex) {
        // Get the ivp node that is the sink of this edge
        val ivpCandidates: Seq[IVPNodeModule] = ivpNodeModules.filter(i => i.node.in.map(_._2).contains(outEdge))
        // Make sure there is only one
        require(ivpCandidates.length == 1, s"internal error")
        val ivpModule: IVPNodeModule = ivpCandidates.head
        // Calculate the input index
        val inIdx: Int = ivpModule.node.in.map(_._2).indexWhere(t => t.equals(outEdge))
        require(
          ivpModule.node.in.map(_._2).count(t => t.equals(outEdge)) == 1,
          s"The number of matched edge parameters is ${ivpModule.node.in.map(_._2).count(t => t.equals(outEdge))}, " +
            s"Connections involve with memory node do not allow duplication"
        )
        // Enqueue the link info
        accLinks += DSALink(
          memModule.nodeType,
          memModule.memNode.getNodeId,
          outIdx,
          ivpModule.nodeType,
          ivpModule.ivpNode.getNodeId,
          inIdx
        )
      }
      // Loop over all input edges
      for ((inEdge, inIdx) <- inEdges.zipWithIndex) {
        // Get the ovp node that is the source of this edge
        val ovpCandicates: Seq[OVPNodeModule] = ovpNodeModules.filter(o => o.node.out.map(_._2).contains(inEdge))
        // Make sure there is only one such ovp
        require(ovpCandicates.length == 1, s"Internal Error")
        val ovpModule: OVPNodeModule = ovpCandicates.head
        // Calculate the output index of that ovp module
        val outIdx: Int = ovpModule.node.out.map(_._2).indexWhere(t => t.equals(inEdge))
        require(
          ovpModule.node.out.map(_._2).count(t => t.equals(inEdge)) == 1,
          s"The number of matched edge parameters if ${ovpModule.node.out.map(_._2).count(t => t.equals(inEdge))}, " +
            s"Connections involve with memory node do not allow duplication"
        )
        // Enqueue the link info
        accLinks += DSALink(
          ovpModule.nodeType,
          ovpModule.ovpNode.getNodeId,
          outIdx,
          memModule.nodeType,
          memModule.memNode.getNodeId,
          inIdx
        )
      }
    }
    // Return
    accLinks.result()
  }

  // Emit JSON for Memory <-> IVP/OVP
  def memEdges2JSON: List[JsObject] = {
    memDSALinks.map(x => x.toJSON)
  }

  // Emit JSON for nodes and edges
  def mem2JSON: JsObject = Json.obj(
    MemNodes.keyName -> JsObject(memNodes2JSON),
    MemEdges.keyName -> JsArray(memEdges2JSON)
  )
}
