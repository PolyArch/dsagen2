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
import dsagen2.mem.bundle.MemReadBundle
import dsagen2.mem.diplomacy.Mem2IVPParameter
import dsagen2.mem.module.MemNodeModule
import dsagen2.sync.bundle.{IVPSetPort, VectorPortStatus}
import dsagen2.sync.config.IVPNodeParameters
import dsagen2.sync.config.SyncKeys.IVPNode
import dsagen2.sync.diplomacy.{IVPNexusNode, JsonParsableSyncNode}
import dsagen2.sync.impl.{InputVectorPortImp, VPNodeModule}
import dsagen2.top.bundle.ReconfPort
import dsagen2.top.config.DSAFixedConfig.CONF_FANOUT
import dsagen2.top.config.DebugKey
import dsagen2.top.config.enumeration.VPImplMode.FullXBarVP
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, InputVectorPort}
import dsagen2.top.diplomacy.ReconfNode
import dsagen2.top.module.DSAGen
import dsagen2.util.StreamUtil.nameVecData
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

/** Input vector port module: includes diplomatic node and hardware implementation
  * @param p CDE parameter for this IVP node
  */
class IVPNodeModule(
  val nodeType: DSAGenNodeType = InputVectorPort,
  val nodeId:   Int
)(
  implicit p: Parameters,
  dsagen:     DSAGen)
    extends LazyModule
    with JsonParsableSyncNode
    with VPNodeModule {
  suggestName(this.toString)

  // Extract Parameter
  def ivpNode: IVPNodeParameters = p(IVPNode).copy(nodeId = nodeId)
  def dummyCompPortParam: CompNodeParameters = CompNodeParameters(
    nodeType = InputVectorPort,
    nodeId = nodeId,
    compBits = 1,
    compUnitBits = 1,
    supportNodeActive = false
  )
  def numInput:  Int = node.in.length
  def numOutput: Int = node.out.length
  // Negotiation : Collect the minimum width needed from compute part
  def minDepthComp(compNodes: Seq[CompNodeParameters]): IVPNodeParameters = {
    // Minimum and total compute node with minimum bits
    val compNodesBits: Seq[Int] =
      if (ivpNode.vpStated) compNodes.drop(1).map(_.compBits)
      else compNodes.map(_.compBits)
    ivpNode.copy(compNodesBits = compNodesBits)
  }

  // Diplomatic Node: Input Vector Port receive multiple MemReadBundle and produce multiple CompBDirBundle
  // it should be NexusNode
  val node: IVPNexusNode = IVPNexusNode(
    // connected memory nodes will not affect compute nodes
    // sync node -> comp node bundle is completely defined by compute node, so dummy port parameter should work
    mem2compFn = _ => dummyCompPortParam,
    // connected compute nodes will not affect memory nodes, so just pass ivp parameters
    comp2memFn = minDepthComp
  )

  // Diplomatic Node: For reconfiguration of input vector port
  val reconfNode: Option[ReconfNode] = {
    if (ivpNode.vpImpl == FullXBarVP)
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
      case None          => (None, None)
    }

    // IVP Setting Port: Stream State, Padding, Port Repeat
    val ivpSetPort: IVPSetPort = IO(Input(new IVPSetPort))

    // IVP Status Port: Report Configuring Status and Busy Status
    val ivpStatus: VectorPortStatus = IO(Output(new VectorPortStatus))

    // Memory Read Ports and Parameters
    val (memReadPorts, memReadParams) = getMemPortsParams

    // Compute Ports and Parameters
    val (compPorts, compParams) = getCompPortsParams

    // Do reset
    val doReset: Bool = (ivpSetPort.valid && ivpSetPort.reset) || (inReconfPort match {
      case Some(value) => value.valid && value.reset
      case None => false.B
    })

    /* ------------------------- Node Implementation          ------------------------- */
    // VP Module Implementation
    val ivpImpl: InputVectorPortImp = Module(
      new InputVectorPortImp(ivpNode, inReconfParam, memReadParams, compParams, dsagen.numIVP)
    )
    ivpImpl.reset := reset.asBool() || doReset

    /* ------------------------- IO Connection            ------------------------- */

    // Memory Read Ports
    require(ivpImpl.memReadPorts.length == memReadPorts.length, s"Memory port number mismatch")
    ivpImpl.memReadPorts.zip(memReadPorts).foreach{ case (i, o) => i <> o}

    // Compute Ports
    require(ivpImpl.compPorts.length == compPorts.length, s"Compute port number mismatch")
    ivpImpl.compPorts.zip(compPorts).foreach{ case (i, o) => i <> o}

    // Status and Setting Port
    ivpImpl.ivpSetPort := ivpSetPort
    ivpStatus := ivpImpl.ivpStatus

    // Reconfiguration Port
    inReconfPort match {
      // Reconfiguration port defined
      case Some(reconfPort) =>
        // Connect to inner port
        ivpImpl.configPort.get := reconfPort
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
  def -->(that: CompNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) =
    that.node.:=(this.node)(p ++ new DownwardConnection, sourceInfo)
  def <--(that: MemNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) =
    this.node := that.readNode
  // Group connection, just a wrapper of -->
  def ==>(group: Seq[CompNodeModule])(implicit p: Parameters, sourceInfo: SourceInfo) =
    group.foreach { that => that.node.:=(this.node)(p ++ new DownwardConnection, sourceInfo) }

  // Memory Side IO Collect
  def getMemPortsParams: (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = {
    // Collect edges from diplomacy node
    val (mRs, mRPs) = node.in.unzip
    // Name it for debug purpose
    if (p(DebugKey)) {
      val mRVs: Seq[UInt] = mRs.zipWithIndex.map { case (x, idx) => nameVecData(x.memData, s"MemRead${idx}_Value") }
      VecInit(mRVs).foreach(dontTouch(_)) // Debug
    }
    // sanity check: input vector port must have at least one memory node connects to it
    require(
      mRs.length == mRPs.length && mRs.nonEmpty,
      s"For input vector port $nodeId, it needs at least one memory node"
    )
    // return
    (mRs, mRPs)
  }

  // Compute Side IO Collect
  def getCompPortsParams: (Seq[CompDirBundle], Seq[CompDirEdgeParameters]) = {
    // first collect bi-direction bundle from diplomacy node
    val (compBDirBundles, compBDirParams) = node.out.unzip
    // Sanity check: must have at least one compute connect to this input vector port
    require(
      compBDirBundles.length == compBDirParams.length && compBDirParams.nonEmpty,
      s"This input vector port $nodeId does not connect to any compute node"
    )
    // Sanity check: make sure all bi-directional connection are actually single directional
    compBDirBundles.foreach { compBDBundle =>
      // The upward bundle from compute node should not be defined
      require(compBDBundle.uPort.isEmpty, s"This input vector port $nodeId should not receive upward bundle")
      // All of the downward bundle should be defined
      require(compBDBundle.dPort.isDefined, s"This input vector port $nodeId does not defined downward bundle")
    }
    // Sanity check: bi-directional parameter should only have one directional parameter
    compBDirParams.foreach { compBDParam =>
      // The upward parameter should not be defined
      require(!compBDParam.uParam.enable, s"This input vector port $nodeId has upward parameter defined")
      // The downward parameter should be defined
      require(compBDParam.dParam.enable, s"This input vector port $nodeId does not have downward parameter")
    }
    // Return
    (compBDirBundles.map(_.dPort.get), compBDirParams.map(_.dParam))
  }
}
