package dsagen2.mem.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.PopCount
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, MemReadNode, MemWriteNode, OVP2MemParameter}
import dsagen2.mem.impl.MemImpl.{broadcastLeftByte, bus2ivps, ivps2tab, ovps2tab}
import dsagen2.mem.module.agent.{DMAReaderModule, DMAWriterModule}
import dsagen2.mem.module.agu.StreamARGU
import dsagen2.mem.module.bus.StreamReadBus
import dsagen2.mem.module.rob.MemReadStreamROB
import dsagen2.mem.module.stab._
import dsagen2.mem.module.tlb.FrontendTLB
import dsagen2.top.config.DSAFixedConfig.memOpRead
import dsagen2.top.module.DSAGen
import dsagen2.util.QueueFPGA
import dsagen2.util.RegUtil.RegNextN
import dsagen2.util.StreamUtil.{disp2stream, strReq2memReq}
import freechips.rocketchip.rocket.TLBPTWIO
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tilelink.TLEdgeOut

/** Direct Memory Access Implementation
  */
object DMAImpl {

  /** Building the circuit for DMA
    *
    * @param strDisp      Dispatch Port from Stream Engine
    * @param memWriteNode Diplomatic Memory Write Node
    * @param memReadNode  Diplomatic Memory Read Node
    * @param reader       DMA TileLink Reader
    * @param writer       DMA TileLink Writer
    * @param ptw          Page Table Writer
    * @param memStatus    Memory Statue Report Port
    * @param memNode      Memory Node Parameter
    * @param dsagen       DSAGen Building Environment
    * @param edge         TileLink Edge from DMA to for TLB to track request
    * @param p            CDE
    */
  def circuit( // Inward, issue instantiated stream entry
    strDisp: StreamDispatchBus,
    // Inward, memory write node
    memWriteNode: MemWriteNode,
    // Outward, memory read node
    memReadNode: MemReadNode,
    // DMA
    reader: DMAReaderModule, // Module
    writer: DMAWriterModule, // Module
    ptw:    TLBPTWIO, // Outward
    // Report Memory Status
    memStatus: MemoryNodeStatus, // Outward
    // Parameters
    memNode: MemNodeParameters,
    dsagen:  DSAGen // Full DSAGen System
  )(
    implicit edge: TLEdgeOut,
    p:             Parameters
  ): Unit = {

    /* ------------------------- Extract Parameters           ------------------------- */

    // The number of byte from system bus per cycle
    val beatBytes: Int = p(SystemBusKey).beatBytes

    /* ------------------------- Parameters Sanity Check      ------------------------- */

    // Do sanity check for memory node
    require(memNode.sanity, s"Memory node ${memNode.getNodeName} does not pass sanity check")

    // The bandwidth of memory node should be same as beatByte for DMA
    require(memNode.bandwidth == beatBytes, s"bandwidth = ${memNode.bandwidth}, beatByte = $beatBytes")

    /* ------------------------- Input / Output               ------------------------- */

    // Memory Read Bundles
    val (memReadPorts, ivpsParam): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Bundles
    val (memWritePorts, ovpsParam): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* ------------------------- Modules                      ------------------------- */

    // Stream Table
    val tab: StreamTable = Module(new StreamTable(memNode, ivpsParam, ovpsParam))

    // Address Generation Unit
    val agu: StreamARGU = Module(new StreamARGU(memNode, ivpsParam, ovpsParam))

    // Reorder Buffer for Reordering Read Request
    val rob: MemReadStreamROB = Module(new MemReadStreamROB(memNode, ivpsParam, ovpsParam))

    // Stream Read Bus
    val bus: StreamReadBus = Module(new StreamReadBus(memNode, ivpsParam, ovpsParam))

    // Stream Response Queue, help to solve physical design problem of FPGA
    val strRspQueue =
      Module(
        new QueueFPGA[StreamResponse](
          new StreamResponse(memNode, ivpsParam, ovpsParam),
          8,
          p(FPGAOverlay),
          dontTouch = false
        )
      )

    // Register per IVP to keep track of the on fly bytes in stream response queue, unit in Byte
    val onflyRegs: Seq[UInt] = memReadPorts.map { r => RegInit(0.U(r.ivpLeftByte.getWidth.W)) }

    // TLB
    val tlb: FrontendTLB = Module(new FrontendTLB(2, entries = 32, maxSize = 32))

    /* ------------------------- Wires                        ------------------------- */

    // Stream Entry from Stream Dispatcher
    val newStrEntry: StreamEntry = WireInit(0.U.asTypeOf(new StreamEntry(memNode, ivpsParam, ovpsParam)))
    disp2stream(newStrEntry, strDisp)

    /* ------------------------- Combination Logic            ------------------------- */

    // Connect Reader to TLB
    tlb.io.clients.head <> reader.module.tlbPort
    tlb.io.clients(1) <> writer.module.tlbPort
    tlb.io.exp.flush_skip := false.B
    tlb.io.exp.flush_retry := false.B

    // Dispatch to Stream Table
    tab.newStrEntry := newStrEntry

    // Connect Write Ports to Stream Table for Data Retrieve
    ovps2tab(tab, memWritePorts)

    // Connect Read Ports to Stream Table for Readiness and used by memory checking
    ivps2tab(tab, memReadPorts)

    // Stream Table selects stream entry to AGU
    agu.forceKill := false.B // TODO: force kill when stream association implemented
    agu.selStrEntry := tab.selStrEntry

    // AGU sends the updated stream entry back to stream table
    tab.updStrEntry := agu.updStrEntry

    // AGU sends the request to ROB
    val agu2robDepth: Int = 2 // TODO: we should remove this in the future
    require(rob.strReqPorts.length == agu.strReqPorts.length && agu.strReqPorts.length == 1)
    val strReqPipe: List[StreamRequest] = RegNextN(agu.strReqPorts.head, stage = agu2robDepth)
    val isRead:     Seq[Bool] = strReqPipe.map(x => x.valid && x.meta.memOperation.get === memOpRead)
    val pipeCount:  UInt = PopCount(isRead)
    rob.strReqPorts.head := strReqPipe.last

    // ROB sends the request to read agent
    reader.module.memRequest := rob.memReqPorts.head

    // AGU sends the request directly write agent
    strReq2memReq(agu.strReqPorts.head, writer.module.memRequest)

    // Reader agent response back to ROB
    rob.memRspPorts.head := reader.module.memResponse

    // Connect ROB's stream response to enqueue of stream response
    strRspQueue.io.enq.bits := rob.strRspPort
    strRspQueue.io.enq.valid := rob.strRspPort.valid
    rob.strAccepted := strRspQueue.io.enq.ready

    // Connect the Dequeue of stream response queue to stream bus's input
    bus.strResponse := strRspQueue.io.deq.bits
    bus.strResponse.valid := strRspQueue.io.deq.valid
    strRspQueue.io.deq.ready := bus.strAccepted

    // Connect IVP capacity and current bytes to ROB
    require(rob.ivpsCapa.length == memReadPorts.length)
    require(rob.ivpsLeftBytes.length == memReadPorts.length)
    require(onflyRegs.length == memReadPorts.length)
    rob.ivpsCapa.zip(memReadPorts).foreach { case (r, read) => r := read.ivpCapa }
    rob.ivpsLeftBytes.zip(broadcastLeftByte(memReadPorts)).zip(onflyRegs).foreach { case ((r, left), onfly) =>
      r := left - onfly
    }

    // Connect stream read bus to each input vector port
    val (deqFires, deqBytes, ivpQs) = bus2ivps(bus, memReadPorts, ivpsParam).unzip3

    // Backpressure from ROB
    tab.readPause match {
      case Some(tabPause) => tabPause := (rob.numValid +& pipeCount) >= memNode.numPendingRequest.U
      case None           =>
    }

    // Backpressure from writer
    tab.writePause match {
      case Some(tabPause) => tabPause := writer.module.writePause
      case None           =>
    }

    // Connect TLB to PTW
    ptw <> tlb.io.ptw

    // Schedule stream based on ROB count
    val robSche: ROBStrScheduler = Module(
      new ROBStrScheduler(tab.numEntry, rob.robSize, ivpsParam.length, agu2robDepth)
    )
    robSche.strValids := tab.strVlds;
    require(robSche.strValids.getWidth == tab.strVlds.getWidth)
    robSche.robValids := rob.robVlds;
    require(robSche.robValids.getWidth == rob.robVlds.getWidth)
    robSche.robStrIDs.zip(rob.robPortIDs).foreach { case (schePort, robPort) =>
      require(schePort.getWidth == robPort.getWidth); schePort := robPort
    }
    require(tab.outerStrValid.get.getWidth == robSche.strRobIssueValids.getWidth)
    tab.outerStrValid.get := robSche.strRobIssueValids
    tab.outerStrAct.get := robSche.readStrActs

    /* -------------------------     Finite State Machine     ------------------------- */

    // Update the on fly register for each enqueue and dequeue
    val enqFire: Bool = strRspQueue.io.enq.fire()
    val enqByte: UInt = PopCount(strRspQueue.io.enq.bits.mask.getOrElse(0.U(1.W)))
    require(onflyRegs.length == ivpsParam.length, s"Length check")
    require(deqFires.length == ivpsParam.length)
    require(deqBytes.length == ivpsParam.length)
    for (ivpIdx <- ivpsParam.indices) {
      // Current amount of bytes
      val currByte: UInt = onflyRegs(ivpIdx)
      // Collect enqueue info
      val enqIvp:          UInt = strRspQueue.io.enq.bits.meta.targetLocalPortId.getOrElse(0.U)
      val ivpDoBroad:      Bool = memReadPorts(ivpIdx).ivpBroadcast.getOrElse(false.B)
      val ivpBroadcaseIvp: UInt = memReadPorts(ivpIdx).ivpBroadcastIVPortId.getOrElse(0.U)
      val enqMatch:        Bool = (enqIvp === ivpIdx.U) || (ivpDoBroad && ivpBroadcaseIvp === enqIvp)
      val doThisEnq:       Bool = enqFire && enqMatch
      // Collect dequeue info
      val deqFire: Bool = deqFires(ivpIdx)
      val deqByte: UInt = deqBytes(ivpIdx)
      when(deqFire && doThisEnq) {
        onflyRegs(ivpIdx) := currByte + enqByte - deqByte
      }.elsewhen(doThisEnq) {
        onflyRegs(ivpIdx) := currByte + enqByte
      }.elsewhen(deqFire) {
        onflyRegs(ivpIdx) := currByte - deqByte
      }.otherwise {
        onflyRegs(ivpIdx) := currByte
      }
    }

    /* ------------------------- Output Connection            ------------------------- */

    // Report the memory status
    memStatus := tab.memStatus
  }
}
