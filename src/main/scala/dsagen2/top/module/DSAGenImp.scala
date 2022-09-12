package dsagen2.top.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.ctrl.module.StreamDispatcherModule
import dsagen2.mem.bundle.MemoryNodeStatus
import freechips.rocketchip.tile.{LazyRoCCModuleImp, RoCCResponse}

class DSAGenImp(dsagen: DSAGen)(implicit p: Parameters) extends LazyRoCCModuleImp(dsagen) {

  require(
    dsagen.numVP > 0,
    s"Decoupled-Spatial Accelerator should have at least one vector port as compute memory interface"
  )
  require(dsagen.numMEM > 0, s"Decoupled-Spatial Accelerator should have at least one memory for storing data")
  require(dsagen.numCOMP > 0, s"Decoupled-Spatial Accelerator should have at least one compute for computation")

  // Extract stream dispatcher from outer
  val dispatcher: StreamDispatcherModule = dsagen.dispatcher

  /** Connect : RoCC wrapper <-> Stream Dispatcher (Control System)
    * including: roccIO : cmd(1), mem(1), busy(1), interrupt(1), exception(1)
    */

  // Stream Specialization ISA
  dispatcher.module.roccCmd <> io.cmd

  // L1D Cache access
  io.mem <> dispatcher.module.roccMem

  // Bothering CPU
  io.busy := dispatcher.module.roccBusy
  io.interrupt := dispatcher.module.roccInterrupt
  dispatcher.module.roccException := io.exception

  /** Connect : RoCC wrapper <-> Memory System
    * including: rdResp(1), PTWs(#dma), tileNode(1)
    */

  // Register Response
  dsagen.getRegResp match {
    case Some(regResp) =>
      // Actual response will arbitrate between stream dispatcher and register engine
      // where stream dispatcher has higher priority
      val arb: Arbiter[RoCCResponse] = Module(new Arbiter[RoCCResponse](new RoCCResponse, 2))
      arb.io.in.head <> dispatcher.module.roccResp
      arb.io.in(1) <> regResp
      io.resp <> arb.io.out
    case None =>
      require(
        dsagen.numREG == 0,
        s"You have ${dsagen.numREG} register engine," +
          s"but you do not need to connect back to register value response?"
      )
      io.resp <> dispatcher.module.roccResp
  }

  // Page Table Walkers
  dsagen.getPTWs match {
    case Some(ptws) =>
      if (ptws.length == io.ptw.length) {
        // Connect page table walker port
        ptws.zip(io.ptw).foreach { case (memPTW, roccPTW) => roccPTW <> memPTW }
      } else {
        // TODO : we should find a way to arbitrate between PTW and DMA
        println(s"Please add : override val nPTWPorts: Int = numDMA ; at the end of your design")
        require(
          requirement = false,
          s"we only support two modes: number of PTWs = number of DMA, so each DMA has it own PTW port; " +
            s"or TODO : there is only one PTW, which means all DMAs arbitrate the PTW. Now" +
            s" we have ${io.ptw.length} ptw ports, #DMA = ${ptws.length}"
        )
      }

    case None =>
      require(requirement = false, s"You don't have DMA, then how do you update compute system?")
  }

  /** Connect : Connect Stream Dispatcher (Control System) <-> Memory System
    * including: strDispPort(1), memoryNodesStatus(#mem)
    */

  // Stream Dispatcher Port
  dsagen.getStrDispBus := dispatcher.module.strDispPort

  // Memory Node Status
  // Get memory node status from DSA System
  val memStatuses: Vec[MemoryNodeStatus] = dsagen.getMemStatus
  dispatcher.module.memsStatus.zip(memStatuses).foreach { case (strDispMemStatus, memStatus) =>
    strDispMemStatus := memStatus
  }
  // Sanity check: the number of memory status port should be same
  require(
    dispatcher.module.memsStatus.length == memStatuses.length && memStatuses.length == dsagen.numMEM,
    s"The number of memory node does not match : " +
      s"Stream dispatcher has ${dispatcher.module.memsStatus.length} memory status port, " +
      s"memory system has ${memStatuses.length} memory status port," +
      s"Global Parameter has ${dsagen.numMEM} memory node"
  )

  /** Connect : Connect Stream Dispatcher (Control System) <-> Sync System
    * including: vecPortsStatus(#vp), sinkPortsSetting(#ivp), sourcePortsSettings(#ovp)
    */

  // Input Vector Port Status reported to Stream Dispatcher
  dispatcher.module.ivpsStatus.zip(dsagen.getIVPsStatus).foreach { case (disp, ivpStatus) => disp := ivpStatus }
  // Sanity check : the amount of input vector port status should be same
  require(
    dispatcher.module.ivpsStatus.length == dsagen.getIVPsStatus.length,
    s"stream dispatcher has ${dispatcher.module.ivpsStatus.length} input vector port status ports," +
      s"but sync system has ${dsagen.getIVPsStatus.length} input vector port status ports, the global parameter" +
      s"records ${dsagen.numIVP} input vector ports"
  )

  // Output Vector Port Status reported to Stream Dispatcher
  dispatcher.module.ovpsStatus.zip(dsagen.getOVPsStatus).foreach { case (disp, ovpStatus) => disp := ovpStatus }
  // Sanity check : the amount of output vector port status should be same
  require(
    dispatcher.module.ovpsStatus.length == dsagen.getOVPsStatus.length,
    s"stream dispatcher has ${dispatcher.module.ovpsStatus.length} output vector port status ports," +
      s"but sync system has ${dsagen.getOVPsStatus.length} output vector port status ports, the global parameter" +
      s"records ${dsagen.numOVP} output vector ports"
  )

  // Input vector port setting from stream dispatcher to input vector ports
  dsagen.getIVPSetting.zip(dispatcher.module.ivpSetPorts).foreach { case (ivpSetting, sdIVPSetting) =>
    // Sanity check: number of bits should be same
    require(ivpSetting.getWidth == sdIVPSetting.getWidth, s"input vector port bit width is different")
    // Connect
    ivpSetting := sdIVPSetting
  }
  // Sanity check: the amount of input vector port setting should be same as stream dispatcher
  require(
    dispatcher.module.ivpSetPorts.length == dsagen.getIVPSetting.length,
    s"stream dispatcher has ${dispatcher.module.ivpSetPorts.length} input vector port setting ports, " +
      s"but sync system has ${dsagen.getIVPSetting.length} input vector port setting ports, " +
      s"There are ${dsagen.numIVP} input vector ports recorded by global parameter"
  )

  // Output vector port settings from stream dispatcher to output vector port
  dsagen.getOVPSetting.zip(dispatcher.module.ovpSetPorts).foreach { case (ovpSetting, sdOVPSetting) =>
    // Sanity check: number of bits should be same
    require(ovpSetting.getWidth == sdOVPSetting.getWidth, s"output vector port setting bit width is different")
    // Connect
    ovpSetting := sdOVPSetting
  }
  // Sanity check: the amount of output vector port setting should be same as that of stream dispatcher
  require(
    dispatcher.module.ovpSetPorts.length == dsagen.getOVPSetting.length,
    s"stream dispatcher has ${dispatcher.module.ovpSetPorts.length} output vector port setting ports, " +
      s"but sync system has ${dsagen.getOVPSetting.length} output vector port setting ports, " +
      s"There are ${dsagen.numOVP} output vector ports recorded by global parameter"
  )

  /** Connect : Connect Memory System <-> Compute System
    * including: Compute Node Status <-- compute system
    */

  // Connect the compute nodes status back to stream dispatcher
  dispatcher.module.compStatuses.zip(dsagen.getCompStatusPorts).foreach { case (disp, compNodeStatus) =>
    require(
      disp.getWidth == compNodeStatus.getWidth,
      s"stream dispatcher compute status is ${disp.getWidth}-bit, but " +
        s"compute node is ${compNodeStatus.getWidth}-bit, missing fields?"
    )
    disp := compNodeStatus
  }

  // Write ADG File
  dsagen.writeADG()
}
