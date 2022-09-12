package dsagen2.sync.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.bundle.CompDirBundle
import dsagen2.comp.bundle.DirectionalConnection.DownwardConnection
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.reconf.{CompReconfEdge, CompReconfProto}
import dsagen2.comp.diplomacy.CompDirEdgeParameters
import dsagen2.comp.module.CompNodeModule
import dsagen2.mem.bundle.MemWriteBundle
import dsagen2.mem.diplomacy.OVP2MemParameter
import dsagen2.mem.module.MemNodeModule
import dsagen2.sync.bundle.{OVPSetPort, VectorPortStatus}
import dsagen2.sync.config.OVPNodeParameters
import dsagen2.sync.config.SyncKeys.OVPNode
import dsagen2.sync.diplomacy.{JsonParsableSyncNode, OVPNexusNode}
import dsagen2.sync.impl.{OutputVectorPortImp, VPNodeModule}
import dsagen2.top.bundle.ReconfPort
import dsagen2.top.config.DSAFixedConfig.CONF_FANOUT
import dsagen2.top.config.enumeration.VPImplMode._
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, OutputVectorPort}
import dsagen2.top.diplomacy.ReconfNode
import dsagen2.top.module.DSAGen
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

/** Output vector port module: includes diplomatic node and hardware implementation
  * @param p CDE parameter for this IVP node
  */
class OVPNodeModule(
  val nodeType: DSAGenNodeType = OutputVectorPort,
  val nodeId:   Int
)(
  implicit p: Parameters,
  dsagen:     DSAGen)
    extends LazyModule
    with JsonParsableSyncNode
    with VPNodeModule {
  suggestName(this.toString)

  // Extract parameter from CDE
  def ovpNode:   OVPNodeParameters = p(OVPNode).copy(nodeId = nodeId)
  def numInput:  Int = node.in.length
  def numOutput: Int = node.out.length
  def dummyCompPortParam: CompNodeParameters = CompNodeParameters(
    nodeType = OutputVectorPort,
    nodeId = nodeId,
    compBits = 1,
    compUnitBits = 1,
    supportNodeActive = false
  )
  // Negotiation : Collect the minimum width needed from compute part
  def minDepthComp(compNodes: Seq[CompNodeParameters]): OVPNodeParameters = {
    // Minimum and total compute node with minimum bits
    val compNodesBits: Seq[Int] =
      if (ovpNode.vpStated) compNodes.drop(1).map(_.compBits)
      else compNodes.map(_.compBits)
    ovpNode.copy(compNodesBits = compNodesBits)
  }

  // Diplomatic Node : Output Vector Port receive multiple CompBDirBundles and produce multiple MemWriteBundles
  // it should be NexusNode
  val node: OVPNexusNode = OVPNexusNode(
    // Connected compute nodes will not affect memory write side, so just pass output vector port parameters
    comp2memFn = minDepthComp,
    // Connected memory nodes will not affect compute nodes
    // Compute side bundles are purely defined by compute nodes since output vector port will not pass parameter back
    // so dummy compute port parameter should work
    mem2compFn = _ => dummyCompPortParam
  )

  // Diplomatic Node for reconfiguration of output vector port
  val reconfNode: Option[ReconfNode] = {
    // XBar based vector port requires
    if (ovpNode.vpImpl == FullXBarVP)
      Some(ReconfNode(dsaModule = this, pFn = { _ => CompReconfProto(this) }, rFn = { _ => CompReconfProto(this) }))
    else None
  }

  // Module Implementation
  lazy val module = new LazyModuleImp(this) {
    /* ------------------------- Input / Output               ------------------------- */

    // Reconfiguration Port and Parameters
    val (inReconfPort, inReconfParam): (Option[ReconfPort], Option[CompReconfEdge]) = reconfNode match {
      case Some(refNode) =>
        require(refNode.in.length == 1, s"Reconfiguration inward port should only be 1, but it is ${refNode.in.length}")
        (Some(refNode.in.head._1), Some(refNode.in.head._2))
      case None => (None, None)
    }

    // Create input port for output vector port setting, TODO: Task Flow support
    val ovpSetPort: OVPSetPort = IO(Input(new OVPSetPort))

    // Create output port for reporting the output vector port status
    val ovpStatus: VectorPortStatus = IO(Output(new VectorPortStatus))

    // Memory Write Ports and Parameters
    val (memWritePorts, memWriteParams) = getMemPortsParams

    // Compute Ports and Parameters
    val (compPorts, compParams) = getCompPortsParams

    // Do reset
    val doReset: Bool = inReconfPort match {
      case Some(value) => value.valid && value.reset
      case None => false.B
    }

    /* ------------------------- Node Implementation          ------------------------- */

    // VP Module Implementation
    val ovpImpl: OutputVectorPortImp = Module(
      new OutputVectorPortImp(ovpNode, inReconfParam, memWriteParams, compParams)
    )
    ovpImpl.reset := reset.asBool() || doReset

    /* ------------------------- IO Connection            ------------------------- */

    // Memory Write Ports
    require(ovpImpl.memWritePorts.length == memWritePorts.length, s"Memory port number mismatch")
    ovpImpl.memWritePorts.zip(memWritePorts).foreach{ case (i, o) => i <> o}

    // Compute Ports
    require(ovpImpl.compPorts.length == compPorts.length, s"Compute port number mismatch")
    ovpImpl.compPorts.zip(compPorts).foreach{ case (i, o) => i <> o}

    // Status and Setting Ports
    ovpImpl.ovpSetPort := ovpSetPort
    ovpStatus := ovpImpl.ovpStatus

    // Reconfiguration Port
    inReconfPort match {
      // Reconfiguration port defined
      case Some(reconfPort) =>
        // Connect to inner port
        ovpImpl.configPort.get := reconfPort
        // Pipeline to other nodes
        val outReconfReg: ReconfPort = RegNext(reconfPort)
        // Sanity check for output reconfiguration port
        require(reconfNode.get.out.length <= CONF_FANOUT, s"Number of output reconfiguration port exceeds " +
          s"the maximum: ${reconfNode.get.out.length} > $CONF_FANOUT")
        // Connect to each
        reconfNode.get.out.foreach{ case (port, _) => port := outReconfReg}
      case None =>
    }
  }

  // Node Connection
  def -->(mem: MemNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) =
    mem.writeNode := this.node
  def <--(comp: CompNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) =
    this.node.:=(comp.node)(p ++ new DownwardConnection, sourceInfo)
  // Group connection, just a wrapper of <--
  def <==(group: Seq[CompNodeModule])(implicit p: Parameters, sourceInfo: SourceInfo) =
    group.foreach { comp => this.node.:=(comp.node)(p ++ new DownwardConnection, sourceInfo) }

  // Collect the bundles from diplomatic node for memory side and sanity check it
  def getMemPortsParams: (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = {
    // collect edges from diplomacy node
    val (mWs, mWPs) = node.out.unzip
    // sanity check: must connects to at least one memory node
    require(
      mWs.length == mWPs.length && mWs.nonEmpty,
      s"For output vector port, it needs to be connected to at least one memory node"
    )
    // Return
    (mWs, mWPs)
  }

  // Collect the bundles from diplomatic node for compute side and sanity check it
  def getCompPortsParams: (Seq[CompDirBundle], Seq[CompDirEdgeParameters]) = {
    // first we should extract the bi-directional bundles
    val (compBDirBundles, compBDirParams) = node.in.unzip
    // sanity check: must have at least one compute node connect to this output vector port
    require(
      compBDirBundles.length == compBDirParams.length && compBDirBundles.nonEmpty,
      s"This output vector port $nodeId does not receive any connection from compute nodes"
    )
    // sanity check: make sure all bi-directional connection are actually single directional
    compBDirBundles.foreach { bdirBundle =>
      // The upward bundle from output vector port should not be defined
      require(bdirBundle.uPort.isEmpty, s"This output vector port $nodeId has upward bundle which should not happen")
      // All downward bundle should be defined
      require(bdirBundle.dPort.isDefined, s"This output vector port $nodeId has non-defined downward bundle")
    }
    // Sanity check: bi-directional parameter should only have one side parameter
    compBDirParams.foreach { bdirParam =>
      // upward parameter should not be defined
      require(!bdirParam.uParam.enable, s"Output vector port $nodeId has upward parameter enabled")
      // downward parameter should be defined
      require(bdirParam.dParam.enable, s"Output vector port $nodeId has non-defined downward parameter")
    }
    // Collect downward directional bundles
    val dirBundles: Seq[CompDirBundle] = compBDirBundles.map(_.dPort.get)
    // Collect downward directional parameters
    val dirParams: Seq[CompDirEdgeParameters] = compBDirParams.map(_.dParam)
    // Return
    (dirBundles, dirParams)
  }
}
