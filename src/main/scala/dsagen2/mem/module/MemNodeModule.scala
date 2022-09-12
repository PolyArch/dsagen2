package dsagen2.mem.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.util.DecoupledIO
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle.MemoryNodeStatus
import dsagen2.mem.config.MemKeys.MemNode
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{MemReadNode, MemWriteNode}
import dsagen2.mem.impl._
import dsagen2.mem.module.agent.{DMAReaderModule, DMAWriterModule}
import dsagen2.sync.module._
import dsagen2.top.config.DebugPrintable
import dsagen2.top.diplomacy.DSANodeModule
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.top.module.DSAGen
import dsagen2.util.CDE.cde2FullJson
import dsagen2.util.NodeUtil.failGen
import firrtl.AttributeAnnotation
import firrtl.annotations.Annotation
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.TLBPTWIO
import freechips.rocketchip.tile.RoCCResponse
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import play.api.libs.json.JsObject

/** LazyModule for single memory node
  *
  * @param p CDE parameters
  */
class MemNodeModule(
  val nodeType: DSAGenNodeType,
  val nodeId:   Int
)(
  implicit p: Parameters,
  dsa:        DSAGen)
    extends LazyModule
    with DSANodeModule
    with DebugPrintable {
  suggestName(this.toString)
  /* ------------------------- Extract Parameters           ------------------------- */

  // extract memory node parameters, nodeId is set when createMemNode
  val memNode: MemNodeParameters = p(MemNode)

  /* ------------------------- Derived Parameters           ------------------------- */

  /* ------------------------- Diplomatic Nodes             ------------------------- */

  // Diplomacy TileLink Node for DMA
  val tlNode: Option[TLIdentityNode] = memNode.nodeType match {
    case DirectMemoryAccess => Some(TLIdentityNode())
    case _                  => None
  }

  // Diplomacy Node for each Memory Node
  val readNode: MemReadNode =
    MemReadNode(write2readFn = _ => memNode, read2writeFn = _ => memNode)
  val writeNode: MemWriteNode =
    MemWriteNode(write2readFn = _ => memNode, read2writeFn = _ => memNode)

  /* ---------- Read / Write Module for DMA ---------- */

  // DMA Read/Write Module Construction
  val dmaReader: Option[DMAReaderModule] = memNode.nodeType match {
    case DirectMemoryAccess => Some(LazyModule(new DMAReaderModule(memNode)))
    case _                  => None
  }
  val dmaWriter: Option[DMAWriterModule] = memNode.nodeType match {
    case DirectMemoryAccess => Some(LazyModule(new DMAWriterModule(memNode)))
    case _                  => None
  }

  // Diplomacy Node Connection for DMA
  (tlNode, memNode.nodeType, dmaReader, dmaWriter) match {
    case (Some(idNode), DirectMemoryAccess, Some(reader), Some(writer)) =>
      val xbarNode = TLXbar()
      xbarNode := reader.node
      xbarNode := writer.node
      idNode := xbarNode
    case _ => // Do nothing if it is not Direct Memory Node
  }

  /* ---------- Module Implementation ----------*/
  lazy val module = new LazyModuleImp(this) {

    /* ------------------------- Parameters Sanity Check      ------------------------- */

    // The memory read node should not have inward edges
    require(readNode.in.isEmpty, s"Memory Read Node is actually SourceNode, why it has inward edges?")

    // The memory write node should not have outward edges
    require(writeNode.out.isEmpty, s"Memory Write Node is actually SinkNode, why it has outward edges?")

    /* ------------------------- Input / Output               ------------------------- */

    /* ---------- From/To Stream Dispatcher ---------- */

    // Stream Dispatch, MUST Port
    val strDisp: StreamDispatchBus = IO(Input(new StreamDispatchBus(dsa)))

    // RoCC response, Reg Node only
    val rdResp: Option[DecoupledIO[RoCCResponse]] = memNode.nodeType match {
      case RegisterEngine => Some(IO(DecoupledIO(new RoCCResponse)))
      case _              => None
    }

    // Page Table Walker, DMA only
    val ptw: Option[TLBPTWIO] = memNode.nodeType match {
      case DirectMemoryAccess => Some(IO(new TLBPTWIO))
      case _                  => None
    }

    // Memory Node Status
    val memStatus: MemoryNodeStatus = IO(Output(new MemoryNodeStatus))

    /* ------------------------- Registers                    ------------------------- */

    /* ------------------------- Modules                      ------------------------- */

    /* ------------------------- Combination Logic            ------------------------- */

    /* ---------- Circuit Building ---------- */
    // Print debug info
    if (printDebug) {
      println(s"$numInput OVP(s) --> ${memNode.getNodeName} --> $numOutput IVP(s)")
    }
    memNode.nodeType match {

      // Implementation of Direct Memory Access
      case DirectMemoryAccess =>
        DMAImpl.circuit(strDisp, writeNode, readNode, dmaReader.get, dmaWriter.get, ptw.get, memStatus, memNode, dsa)(
          tlNode.get.edges.out.head,
          p
        )

      // Implementation of Scratchpad Memory
      case ScratchpadMemory => SPMImpl.circuit(strDisp, writeNode, readNode, memStatus, memNode, dsa)

      // Implementation of Generate Engine
      case GenerateEngine => GENImpl.circuit(strDisp, writeNode, readNode, memStatus, memNode)

      // Implementation of Register Engine
      case RegisterEngine => REGImpl.circuit(strDisp, writeNode, readNode, rdResp.get, memStatus, memNode)

      // Implementation of Recurrence Engine
      case RecurrenceEngine => RECImpl.circuit(strDisp, writeNode, readNode, memStatus, memNode)

      // Implementation of Discard Engine
      case DiscardEngine => DISImpl.circuit(strDisp, writeNode, readNode, memStatus, memNode)

      // Error on generation
      case errorType: Any => require(requirement = false, s"Not supported Memory Node Type = $errorType")
    }

    // Count the number of input or output
    def numInput: Int = {
      require(writeNode.edges.out.isEmpty);
      writeNode.edges.in.length
    }

    def numOutput: Int = {
      require(readNode.edges.in.isEmpty);
      readNode.edges.out.length
    }
  }

  /* ---------- Connection Function ---------- */

  // Connection Function
  def -->(ivp: IVPNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) = ivp.node := this.readNode

  def <--(ovp: OVPNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) = this.writeNode := ovp.node

  // Convert Compute Node to JSON Object
  def toJSON: JsObject = {
    nodeType match {
      case DirectMemoryAccess => cde2FullJson(Seq(MemNode), Nil, writeNode.in.length, readNode.out.length)
      case ScratchpadMemory   => cde2FullJson(Seq(MemNode), Nil, writeNode.in.length, readNode.out.length)
      case RecurrenceEngine   => cde2FullJson(Seq(MemNode), Nil, writeNode.in.length, readNode.out.length)
      case DiscardEngine      => cde2FullJson(Seq(MemNode), Nil, writeNode.in.length, readNode.out.length)
      case GenerateEngine     => cde2FullJson(Seq(MemNode), Nil, writeNode.in.length, readNode.out.length)
      case RegisterEngine     => cde2FullJson(Seq(MemNode), Nil, writeNode.in.length, readNode.out.length)
      case t: DSAGenNodeType => failGen(t); JsObject.empty
    }
  }
}
