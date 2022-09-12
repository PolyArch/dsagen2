package dsagen2.comp.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.comp.bundle._
import dsagen2.comp.config.CompKeys._
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.reconf.CompReconfProto
import dsagen2.comp.diplomacy.{CompNexusNode, DAGConnectCompNode, JsonParsableCompNode}
import dsagen2.top.bundle.ReconfPort
import dsagen2.top.config.DSAFixedConfig.CONF_FANOUT
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.top.diplomacy.{DSANodeModule, ReconfNode}
import dsagen2.top.module.DSAGen
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

import scala.collection.Set

// Comp Node
class CompNodeModule(
  val nodeType:     DSAGenNodeType,
  val nodeId:       Int,
  val globalCompId: Int
)(
  implicit p: Parameters,
  dsagen:     DSAGen)
    extends LazyModule
    with FormalizeCompBDirBundle
    with DAGConnectCompNode
    with JsonParsableCompNode
    with DSANodeModule {

  /* ------------------------- Extract Parameters           ------------------------- */

  val compNode: CompNodeParameters = p(CompNode).copy(nodeId = nodeId)
  suggestName(compNode.nodeType.toString + s"_$getComment" + nodeId)

  /* ------------------------- Derived Parameters           ------------------------- */

  /* ------------------------- Diplomatic Node              ------------------------- */

  // Compute Diplomatic Node
  val node: CompNexusNode = CompNexusNode(
    primaryFn = { _ => compNode }, // just pass its own parameter
    replicaFn = { _ => compNode } // just pass its own parameter
  )

  // Reconfiguration Diplomatic Node
  val reconfNode: Option[ReconfNode] =
    Some(ReconfNode(dsaModule = this, pFn = { _ => CompReconfProto(this) }, rFn = { _ => CompReconfProto(this) }))

  // Module Implementation
  lazy val module = new LazyModuleImp(this) {

    /* ------------------------- Input / Output               ------------------------- */

    // Node configuration
    require(
      reconfNode.get.in.length == 1,
      s"$nodeType $nodeId can only receive one reconfiguration input port," +
        s"but it receive ${reconfNode.get.in.length}"
    )
    val (inReconfPort, inReconfParam) = reconfNode.get.in.head

    // Node status
    val compStatus: CompNodeStatus = IO(Output(new CompNodeStatus))

    /* ------------------------- Node Implementation          ------------------------- */

    // Take the input and output hardware bundle
    val compImpl: CompModuleImpl = nodeType match {
      // Generate Implementation for Switch
      case Switch => Module(new SwitchModuleImpl(compNode, inReconfParam, inParams, outParams))
      // Generate Implementation for Processing Element
      case ProcessingElement => Module(new ProcessingElementImpl(compNode, inReconfParam, inParams, outParams))
      // Error: unknown compute node type
      case errType: DSAGenNodeType => require(requirement = false, s"$errType is not compute node"); null
    }

    // Reset signal
    compImpl.reset := reset.asBool() || (inReconfPort.valid && inReconfPort.reset)

    /* ------------------------- Output Connection            ------------------------- */

    // Connect Module IO to Implementation IO
    connectBitsBundleSeq(output_bundles, compImpl.compOutPorts)
    connectBitsBundleSeq(compImpl.compInPorts, input_bundles)

    // Connect status
    compStatus := compImpl.compStatus

    // Pass the reconfiguration info to the downstream nodes
    require(
      reconfNode.get.out.length <= CONF_FANOUT,
      s"$nodeType $nodeId is reconfiguring ${reconfNode.get.out.length} nodes, " +
        s"which exceeds the twice of max number of fanout $CONF_FANOUT"
    )

    // Connect configuration port and pipeline it down to other nodes
    compImpl.configPort := inReconfPort
    val outReconfReg: ReconfPort = RegNext(inReconfPort)
    reconfNode.get.out.foreach { case (outReconfPort, _) =>
      // Connect the reconfiguration port in a pipeline way
      outReconfPort := outReconfReg
    }
  }

  /* ------------------------- Utility                      ------------------------- */

  // Connect Bits Bundle Sequence
  def connectBitsBundleSeq(sinks: Seq[CompDirBundle], sources: Seq[CompDirBundle]): Unit = {
    sinks.zip(sources).foreach { case (sink, source) =>
      // Valid Ready, Control, Data
      sink.valid.get := source.valid.get
      source.ready.get := sink.ready.get
      sink.ctrl.get <> source.ctrl.get
      CompDataConnect(sink.data.get, source.data.get)
    }
  }

  // Patch Comment to name of node
  def getComment: String = if (compNode.comment == "") "" else compNode.comment + "_"

  // Get the operation+datatype set, switch will return empty set
  def opDataTypeSet: Set[DsaOperDataType] = {
    nodeType match {
      case ProcessingElement =>
        // Get Operation Parameter from CDE
        p.lift(DsaOperations) match {
          case Some(aluParam) => aluParam.opDataTypeSet
          case None =>
            require(requirement = false, s"PE does not contains operation parameter")
            Set.empty
        }
      case Switch => Set.empty
      case errType: Any =>
        require(requirement = false, s"Node type $errType is not compute node")
        Set.empty
    }
  }
}
