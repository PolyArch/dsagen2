package dsagen2.mem.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, MemReadNode, MemWriteNode, OVP2MemParameter}
import dsagen2.mem.impl.MemImpl.{broadcastLeftByte, bus2ivps, ivps2tab, ovps2tab}
import dsagen2.mem.module.agu.StreamARGU
import dsagen2.mem.module.bank._
import dsagen2.mem.module.bus.StreamReadBus
import dsagen2.mem.module.rob.MemReadStreamROB
import dsagen2.mem.module.stab._
import dsagen2.mem.module.xbar.{MemReqXBar, MemRspXBar}
import dsagen2.top.config.DSAFixedConfig.memOpRead
import dsagen2.top.module.DSAGen
import dsagen2.util.RegUtil.RegNextN
import dsagen2.util.StreamUtil.{disp2stream, strReq2memReq}

object SPMImpl {
  def circuit(
    strDisp: StreamDispatchBus,
    // Inward, memory write node
    memWriteNode: MemWriteNode,
    // Outward, memory read node
    memReadNode: MemReadNode,
    // Report Memory Status
    memStatus: MemoryNodeStatus, // Outward
    // Outside System and Global Parameter
    memNode: MemNodeParameters,
    dsagen:  DSAGen // Full DSAGen System
  )(
    implicit p: Parameters
  ): Unit = {

    /* ------------------------- Extract Parameters           ------------------------- */

    /* ------------------------- Derived Parameters           ------------------------- */

    // Repeater Depth between AGU and Bank
    val aguReqPipeDepth: Int = 2

    /* ------------------------- Parameters Sanity Check      ------------------------- */

    // Do sanity check for memory node
    require(memNode.sanity, s"Memory node ${memNode.getNodeName} does not pass sanity check")

    // Node Type check
    require(memNode.isSPM, s"You went to the wrong door, this is SPM impl, but you have ${memNode.nodeType}")

    /* ------------------------- Input / Output               ------------------------- */

    // Memory Read Bundles
    val (memReadPorts, ivpsParam): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Bundles
    val (memWritePorts, ovpsParam): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* ------------------------- Registers                    ------------------------- */

    /* ------------------------- Modules                      ------------------------- */

    // Stream Table
    val tab: StreamTable = Module(new StreamTable(memNode, ivpsParam, ovpsParam))

    // Address Generation Unit
    val agu: StreamARGU = Module(new StreamARGU(memNode, ivpsParam, ovpsParam))

    // Reorder Buffer for Indirect-enabled Scratchpad Reordering Read Request
    val rob: Option[MemReadStreamROB] = {
      if (memNode.numSpmBank > 1 && memNode.IndirectIndexStream) {
        Some(Module(new MemReadStreamROB(memNode, ivpsParam, ovpsParam)))
      } else None
    }

    // Request Crossbar to route the request to correct bank and solve bank conflict
    val reqXbar: Option[MemReqXBar] =
      if (memNode.numSpmBank > 1 && memNode.IndirectIndexStream) {
        Some(Module(new MemReqXBar(memNode)))
      } else None

    // Banked Scratchpad
    val bankSPMs: Seq[Scratchpad] = Seq.fill(memNode.numSpmBank)(Module(new Scratchpad(memNode, dsagen)))

    // Response Crossbar to route the response from each bank to correct vector position
    val rspXbar: Option[MemRspXBar] =
      if (memNode.numSpmBank > 1 && memNode.IndirectIndexStream) {
        Some(Module(new MemRspXBar(memNode)))
      } else None

    // Stream Read Bus, take the stream response from ROB or
    // for linear scratchpad, banked memory directly, and send it to mem read port
    val bus: StreamReadBus = Module(new StreamReadBus(memNode, ivpsParam, ovpsParam))

    /* ------------------------- Wires                        ------------------------- */

    // Stream Entry from Stream Dispatcher
    val newStrEntry: StreamEntry = WireInit(0.U.asTypeOf(new StreamEntry(memNode, ivpsParam, ovpsParam)))
    disp2stream(newStrEntry, strDisp)

    /* ------------------------- Combination Logic            ------------------------- */

    // Dispatch to Stream Table
    tab.newStrEntry := newStrEntry

    // Connect Write Ports to Stream Table for Data Retrieve
    ovps2tab(tab, memWritePorts)

    // Connect Read Ports to Stream Table for Readiness and used by memory checking
    ivps2tab(tab, memReadPorts)

    // Stream Table selects stream entry to AGU
    agu.forceKill := false.B // TODO: force kill when stream association is implemented
    agu.selStrEntry := tab.selStrEntry

    // AGU sends the updated stream entry back to stream table
    tab.updStrEntry := agu.updStrEntry

    // AGU -> (Request XBar) -> Scratchpad Banks -> (Response XBar) -> (ROB) -> Stream Response
    // Pipeline the stream request
    val strReqPipe:  List[Vec[StreamRequest]] = RegNextN(agu.strReqPorts, aguReqPipeDepth)
    val strReqPorts: Vec[StreamRequest] = strReqPipe.last
    // Count the number of read operation in pipe and stream request port from AGU
    val isRead:    Seq[Bool] = strReqPipe.map(x => x.head.valid && x.head.meta.memOperation.get === memOpRead)
    val pipeCount: UInt = PopCount(isRead)
    // If it is indirect enabled scratchpad and have multiple bank
    if (memNode.isSPM && memNode.numSpmBank > 1 && memNode.IndirectIndexStream) {
      // Indirect Banked Scratchpad
      (rob, reqXbar, rspXbar) match {
        case (Some(rob), Some(reqXbar), Some(rspXbar)) =>
          // AGU sends the request to ROB
          require(rob.strReqPorts.length == strReqPorts.length)
          rob.strReqPorts.zip(strReqPorts).foreach { case (robReq, aguReq) => robReq := aguReq }

          // Connect ROB to Request XBar
          // TODO: Check whether ROB will just forward request that is not read
          require(reqXbar.vecInput.length == rob.memReqPorts.length)
          reqXbar.vecInput.zip(rob.memReqPorts).foreach { case (xbarReq, robReq) =>
            xbarReq := robReq
          }

          // Connect Memory Request Port of Request XBar to Banked Scratchpad
          require(bankSPMs.length == reqXbar.vecOutput.length)
          bankSPMs.zip(reqXbar.vecOutput).zip(reqXbar.vecOutputReady).foreach { case ((bank, request), requestReady) =>
            bank.memRequest := request
            requestReady := bank.reqReady
          }

          // Connect Memory Response from Banked Scratchpad to Response XBar
          require(rspXbar.vecInput.length == bankSPMs.length)
          rspXbar.vecInput.zip(bankSPMs).foreach { case (response, bank) =>
            response := bank.memResponse
          }

          // Connect Response XBar back to ROB
          require(rob.memRspPorts.length == rspXbar.vecOutput.length)
          rob.memRspPorts.zip(rspXbar.vecOutput.zip(rspXbar.vecOutputReady)).foreach {
            case (robRsp, (memRsp, memRspReady)) =>
              robRsp := memRsp
              memRspReady := true.B // ROB should not send backpressure here
          }

          // Schedule stream based on ROB count
          val robSche: ROBStrScheduler = Module(
            new ROBStrScheduler(tab.numEntry, rob.robSize, ivpsParam.length, aguReqPipeDepth)
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

          // Connect IVP capacity and current bytes to ROB
          require(rob.ivpsCapa.length == memReadPorts.length)
          require(rob.ivpsLeftBytes.length == memReadPorts.length)
          rob.ivpsCapa.zip(memReadPorts).foreach { case (r, read) => r := read.ivpCapa }
          rob.ivpsLeftBytes.zip(broadcastLeftByte(memReadPorts)).foreach { case (r, left) => r := left }

          // Connect stream response to Stream Read Bus
          rob.strAccepted := bus.strAccepted
          bus.strResponse := rob.strRspPort
        case _ =>
      }
    } else {
      // Linear Banked Scratchpad
      // Make sure we do not have this stuffs
      require(rob.isEmpty && reqXbar.isEmpty && rspXbar.isEmpty)

      // AGU sends the stream request to each banks directly
      require(bankSPMs.length == strReqPorts.length)
      bankSPMs.zip(strReqPorts).zipWithIndex.foreach { case ((bank, request), reqIdx) =>
        strReq2memReq(request, bank.memRequest)
        // Connect Request Index
        bank.memRequest.reqIdx match {
          case Some(rIdx) => rIdx := reqIdx.U
          case None       =>
        }
      }

      // Bank sends the memory response combined with one cycle delayed stream request's meta info to bus
      val bankMemAggRsp: StreamResponse = WireInit(0.U.asTypeOf(new StreamResponse(memNode, ivpsParam, ovpsParam)))

      // Connect Valid bit, although I think they will be valid at the same time, I still andR them
      // Valid only when read
      bankMemAggRsp.valid := VecInit(bankSPMs.map(_.memResponse.valid)).asUInt().andR()

      // Connect meta info, I believe meta info is identical for all banks, so I use delayed first stream request
      val validPipe: List[Bool] = RegNextN(strReqPorts.head.valid, aguReqPipeDepth)
      val metaPipe:  List[StreamMeta] = RegNextN(strReqPorts.head.meta, aguReqPipeDepth)
      bankMemAggRsp.meta := metaPipe.last

      // Connect state, I believe stream state info is like meta info
      bankMemAggRsp.state match {
        case Some(value) =>
          value := RegNextN(strReqPorts.head.state.getOrElse(0.U.asTypeOf(new StreamState)), aguReqPipeDepth).last
        case None =>
      }

      // Connect the address, although I feel it will never be used
      bankMemAggRsp.addr match {
        case Some(addr) => addr := bankSPMs.head.memResponse.addr
        case None       =>
      }

      // Connect the mask
      bankMemAggRsp.mask match {
        case Some(mask) =>
          val aggMask: UInt = VecInit(bankSPMs.map(_.memResponse.mask)).asUInt()
          require(mask.getWidth == aggMask.getWidth)
          mask := aggMask
        case None =>
      }

      // Calculate the active bit for read stream in request pipe and memory pipe
      val outerReadAct: Seq[Bool] = for (entryIdx <- ivpsParam.indices) yield {
        // Check whether there is valid request that is read and port match in request pipe
        val matchInReqPipe: Bool = VecInit(strReqPipe.map { req =>
          req.head.valid && req.head.meta.memOperation.get === memOpRead &&
          req.head.meta.targetLocalPortId.getOrElse(0.U) === entryIdx.U
        }).asUInt().orR()
        // Check whether there is valid request that is read and port match in read pipe
        val matchInMemPipe: Bool = VecInit(validPipe.zip(metaPipe).map { case (v, mReq) =>
          v && mReq.memOperation.get === memOpRead && mReq.targetLocalPortId.getOrElse(0.U) === entryIdx.U
        }).asUInt().orR()
        matchInReqPipe || matchInMemPipe
      }

      // Calculate the valid bit that indicate whether read stream can be issued based on port ready and request queue
      // info
      val outerStrValid: Seq[Bool] = for (entryIdx <- 0 until (ivpsParam.length + ovpsParam.length)) yield {
        if (entryIdx < ivpsParam.length) {
          // Read Stream can only be issued if
          memReadPorts(entryIdx).ivpReady && !outerReadAct(entryIdx)
        } else {
          // OVP cannot be stopped by status in request pipe
          true.B
        }
      }

      // Occupy IVP if request on flight
      require(tab.outerStrAct.isDefined)
      require(tab.outerStrAct.get.getWidth == outerReadAct.length)
      tab.outerStrAct.get := VecInit(outerReadAct).asUInt()

      // Stop stream issue if port not ready or request on flight
      require(tab.outerStrValid.isDefined)
      require(tab.outerStrValid.get.getWidth == outerStrValid.length)
      tab.outerStrValid.get := VecInit(outerStrValid).asUInt()

      // Connect data
      bankMemAggRsp.readData match {
        case Some(data) =>
          val aggData: UInt = VecInit(bankSPMs.map(_.memResponse.data)).asUInt()
          require(data.getWidth == aggData.getWidth)
          data := aggData
        case None =>
      }

      // Connect bank aggregated stream response to bus
      bus.strResponse := bankMemAggRsp
    }

    // Connect stream read bus to each input vector port
    bus2ivps(bus, memReadPorts, ivpsParam)

    // Not all scratchpad banks are ready
    val requestPause: Bool = !VecInit(bankSPMs.map(_.reqReady)).asUInt().andR()

    // Stop read: ROB is full or stop by atomic update
    tab.readPause match {
      case Some(tabPause) =>
        tabPause := {
          if (rob.isEmpty) requestPause
          else requestPause || (rob.get.numValid +& pipeCount) >= memNode.numPendingRequest.U
        }
      case None =>
    }

    // Backpressure: from writer, None, scratchpad
    tab.writePause match {
      case Some(tabPause) => tabPause := false.B // scratchpad will never be pause by write only
      case None           => // Only DMA or indirect scratchpad will be paused by write
    }

    // Backpressure: from request XBar
    tab.reqXBarPause match {
      case Some(tabPause) =>
        require(reqXbar.isDefined)
        tabPause := reqXbar.get.pause
      case None =>
    }

    // Backpressure: from response XBar
    tab.rspXBarPause match {
      case Some(tabPause) =>
        require(rspXbar.isDefined)
        tabPause := rspXbar.get.pause
      case None =>
    }

    /* ------------------------- Output Connection            ------------------------- */

    // Report the memory status
    memStatus := tab.memStatus
  }
}
