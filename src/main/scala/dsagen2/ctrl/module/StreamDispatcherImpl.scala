package dsagen2.ctrl.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.bundle.CompNodeStatus
import dsagen2.comp.config.reconf.CompReconfEdge
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.ctrl.bundle.decode.{LinIndRs1Decode, RecRs1Decode}
import dsagen2.ctrl.bundle.isa._
import dsagen2.ctrl.bundle.{CtrlNodeStatus, StreamDispatchBus, SyncEntry}
import dsagen2.ctrl.config.DSARegister._
import dsagen2.ctrl.config.StrDispParams
import dsagen2.mem.bundle.MemoryNodeStatus
import dsagen2.sync.bundle.{IVPSetPort, OVPSetPort, VectorPortStatus}
import dsagen2.top.bundle.ReconfPort
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.config.DebugKey
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.top.module.{DSAGen, DSAMonitor}
import dsagen2.util.StreamUtil.validReduceMin
import dsagen2.util.{QueueFPGA, WithQueueIO}
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse}

/** Stream Dispatcher that bridges CPU and Memory System + Sync System
  *
  * @param p CDE Parameter
  */
class StreamDispatcherImpl(
  val outer: StreamDispatcherModule,
  val dsa:   DSAGen
)(
  implicit p: Parameters)
    extends LazyModuleImp(outer)
    with DSARFArch {
  suggestName(s"StreamDispatcher")

  /* ------------------------- Extract Parameters           ------------------------- */

  // Extract controller parameter
  val ctrlParam: StrDispParams = outer.ctrlParam

  /* ------------------------- Derived Parameters           ------------------------- */

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  /* ------------------------- Input / Output               ------------------------- */

  /* ------ To/From Host CPU (RoCC wrapper) ------ */

  // RoCC Command, instruction
  val roccCmd: DecoupledIO[RoCCCommand] = IO(Flipped(DecoupledIO(new RoCCCommand)))

  // L1D Cache Access, loading the config bitstream
  val roccMem: HellaCacheIO = IO(new HellaCacheIO)

  // Output, Busy, TODO: we have not talk about this yet
  val roccBusy: Bool = IO(Output(Bool()))

  // Output, Interrupt
  val roccInterrupt: Bool = IO(Output(Bool()))

  // Input, Exception, TODO: we have not talk about this yet
  val roccException: Bool = IO(Input(Bool()))

  // Output, RoCC Response, output when barrier is dequeued
  val roccResp: DecoupledIO[RoCCResponse] = IO(Decoupled(new RoCCResponse))

  // DontCare cmd, mem, busy, Interrupt by default
  // Also don't care will be overwritten by following connections, so dont worry
  // there are some memory signals that I don't know the function so just leave it
  roccMem := DontCare
  roccInterrupt := DontCare

  /* ------ To/From Memory System ------ */

  // Stream Dispatch Bus
  val strDispPort: StreamDispatchBus = IO(Output(new StreamDispatchBus(dsa)))

  // Memory Nodes Status
  val memsStatus: Vec[MemoryNodeStatus] = IO(Input(Vec(dsa.numMEM, new MemoryNodeStatus)))

  /* ------ To/From Sync System ------ */

  // Input Vector Port Status
  val ivpsStatus: Vec[VectorPortStatus] = IO(Input(Vec(dsa.numIVP, new VectorPortStatus)))

  // Output Vector Port Status
  val ovpsStatus: Vec[VectorPortStatus] = IO(Input(Vec(dsa.numOVP, new VectorPortStatus)))

  // Input Vector Port Setting
  val ivpSetPorts: Seq[IVPSetPort] = Seq.fill(dsa.numIVP)(IO(Output(new IVPSetPort)))

  // Output Vector Port Setting
  val ovpSetPorts: Seq[OVPSetPort] = Seq.fill(dsa.numOVP)(IO(Output(new OVPSetPort)))

  /* ------ To Comp System ------ */

  // Config Ports to each node of compute system
  require(
    outer.initReconfNode.out.length <= CONF_FANOUT,
    s"Stream Dispatcher should have up to 2 x $CONF_FANOUT configuration output ports, " +
      s"but it has ${outer.initReconfNode.out.length} now"
  )
  val (reconfPorts, reconfParams): (Seq[ReconfPort], Seq[CompReconfEdge]) = outer.initReconfNode.out.unzip

  // The Status of each Compute Node in Compute System
  val compStatuses: Vec[CompNodeStatus] = IO(Input(Vec(dsa.numCOMP, new CompNodeStatus)))

  /* ------------------------- Registers                    ------------------------- */

  // Bool Register trigger bitstream reloading
  val reloadBitstream: Bool = RegInit(false.B)

  // Counter Register to track the config bitstream loading
  // Counter to track the number of configuration requested
  val reqConfCtr: Counter = Counter(MAX_CONFIG_BITSTREAM_XLEN_SIZE + 1)
  // Counter to track the number of configuration responded
  val rspConfCtr: Counter = Counter(MAX_CONFIG_BITSTREAM_XLEN_SIZE + 1)
  require(dsa.reconfDepth > 0, s"Depth of reconfiguration net should be positive, but it is ${dsa.reconfDepth}")
  // Counter to track the number of configuration configured
  val configCtr: Counter = Counter(dsa.reconfDepth)

  // FIFO Queue that store the synchronization command entry
  val syncQueue: WithQueueIO[SyncEntry] = Module(
    new QueueFPGA(new SyncEntry(ctrlParam), ctrlParam.dispQueueDepth, p(FPGAOverlay))
  )

  // Register Array that holds pending full stream entry
  // TODO: the valid bit is useless (should be always true), can be optimized out,
  //  use [[strEntryValids]] instead
  val strEntryQueue: Vec[StreamDispatchBus] =
    RegInit(VecInit(Seq.fill(ctrlParam.dispQueueDepth)(0.U.asTypeOf(new StreamDispatchBus(dsa)))))

  // Register Array that holds valid bit for each full stream entry
  val strEntryValids: Seq[Bool] = Seq.fill(ctrlParam.dispQueueDepth)(RegInit(false.B))

  // Register Array that holds the stream creation ID
  val strCreateIDBase: Seq[UInt] = Seq.fill(ctrlParam.dispQueueDepth)(RegInit(0.U(ctrlParam.strCreationIDBits.W)))

  // Stream Creation ID carry bit
  // when a new stream entry got issue, its id is zero but there are other valid entries whose id is not zero
  // then its carry bit will be set to one, indicating that this creation id is the largest one
  // when all valid entry has carry, then the carry bit will be reset
  // the order of stream creation should be determined by strCreationIDbase and this carry bit
  val strCreateIDCarry: Seq[Bool] = Seq.fill(ctrlParam.dispQueueDepth)(RegInit(false.B))

  // Counter Register that tag each stream entry to determine the order of creation for each stream entry, just base order
  val strCreateIDBaseCtr: Counter = Counter(ctrlParam.maxStrCreationID)

  // A sequence of counter that record the number of cycle that entry is ready
  // the purpose is to prevent miss-calculate conflict when stream entry is issued from dispatcher to stream engine
  val strRdyCtrs: Seq[Counter] = Seq.fill(ctrlParam.dispQueueDepth)(Counter(ctrlParam.dispStage + 1))

  // Create the performance monitor if Debug Mode is enabled
  val dsaMonitor: Option[DSAMonitor] = if (p(DebugKey)) Some(Module(new DSAMonitor(dsa))) else None

  /* ------------------------- Wires                        ------------------------- */

  // The status of stream dispatcher
  val dispStatus: CtrlNodeStatus = WireInit(0.U.asTypeOf(new CtrlNodeStatus))

  // CPU Source Register Value
  val cpuRs1Val: UInt = roccCmd.bits.rs1
  val cpuRs2Val: UInt = roccCmd.bits.rs2

  // Decoded Wire of Command Instruction
  val ssLin:     SSLinStrm = roccCmd.bits.inst.asTypeOf(new SSLinStrm)
  val ssInd:     SSIndStrm = roccCmd.bits.inst.asTypeOf(new SSIndStrm)
  val ssRec:     SSRecStrm = roccCmd.bits.inst.asTypeOf(new SSRecStrm)
  val ssPortCfg: SSPortCfg = roccCmd.bits.inst.asTypeOf(new SSPortCfg)
  val ssParaCfg: SSParaCfg = roccCmd.bits.inst.asTypeOf(new SSParaCfg)
  val ssRecv:    SSRecv = roccCmd.bits.inst.asTypeOf(new SSRecv)
  val ssWait:    SSWait = roccCmd.bits.inst.asTypeOf(new SSWait)
  val ssStat:    SSStat = roccCmd.bits.inst.asTypeOf(new SSStat)

  // Specific instruction is valid
  val isLinStr:  Bool = roccCmd.valid && ssLin.isLinStrm
  val isIndStr:  Bool = roccCmd.valid && ssInd.isIndStrm
  val isRecStr:  Bool = roccCmd.valid && ssRec.isRecStrm
  val isParaCfg: Bool = roccCmd.valid && ssParaCfg.isParaCfg
  val isPortCfg: Bool = roccCmd.valid && ssPortCfg.isPortCfg
  val isRecv:    Bool = roccCmd.valid && ssRecv.isRecv
  val isWait:    Bool = roccCmd.valid && ssWait.isWait
  val isStat:    Bool = roccCmd.valid && ssStat.isStat

  // Receive any DSA Command
  val isDSA: Bool = isLinStr || isIndStr || isRecStr || isParaCfg || isPortCfg || isRecv || isWait || isStat

  // Decoded CPU rs1 Value for linear/indirect stream
  val linIndDecRs1Val: LinIndRs1Decode = cpuRs1Val.asTypeOf(new LinIndRs1Decode)

  // Decoded CPU rs1 Value for recurrence stream
  val recDecRs1Val: RecRs1Decode = cpuRs1Val.asTypeOf(new RecRs1Decode)

  // Wire that holds the full stream entry when stream instantiation command is received
  val newStrEntry: StreamDispatchBus = WireInit(0.U.asTypeOf(new StreamDispatchBus(dsa)))

  // The statistic to be collected is passed by CPU rs1 value
  val statIdx: UInt = cpuRs1Val

  // Stream Command Entry Queue is full
  val strQueueFull: Bool = VecInit(strEntryValids).asUInt().andR()

  // Whether there is a wait command blocking the issue of stream entry directly from RoCC
  // which will result in entry from RoCC enter command queue
  val syncBlocking: Bool = WireInit(false.B)

  // Oldest wait command
  val oldestWaitBits: SyncEntry = syncQueue.io.deq.bits

  // Whether new Stream Entry's related port are busy
  val newStrEntryPortBusy: Bool =
    streamPortsBusy(newStrEntry, ivpsStatus, ovpsStatus, strEntryQueue, strEntryValids, queueCheck = true)

  // Port Busy Check for each entry in Stream Entry Queue
  val queueStrPortBusy: Vec[Bool] = VecInit(strEntryQueue.zip(strEntryValids).map { case (entry, valid) =>
    // TODO: the port conflict check should also be done for queue stream entry, this is a known problem
    //  but it is not causing problem, due to the current issue is actually in order
    //  so the conflict is solved by stream creation order, if we are heading to true out of order, we should remove it
    streamPortsBusy(entry, ivpsStatus, ovpsStatus) && valid
  })

  // Valid Entry whose related ports are not busy in Stream Entry Queue and valid (ready to dispatch)
  val queueStrEntryAvails: Vec[Bool] = VecInit(
    queueStrPortBusy.zip(strEntryValids).map { case (busy, v) => !busy && v }
  )

  // Stream Entry Empty
  val strEntryEmpties: Vec[Bool] = VecInit(strEntryValids.map(!_))
  val queuyIsEmpty:    Bool = strEntryEmpties.asUInt().andR()

  // Priority Encoder that finds the first place for new Stream Entry
  val locationNewStrEntry: UInt = PriorityEncoder(strEntryEmpties.asUInt())

  // Stream Creation ID, wire that combine the stream ID carry and stream ID base
  val strCreateID: Vec[UInt] = VecInit(strCreateIDCarry.zip(strCreateIDBase).map { case (c, int) => Cat(c, int) })

  // Whether all valid stream entry has carry bit set: indication of resetting all carry bit
  val carryReset: Bool = VecInit(strEntryValids.zip(strCreateIDCarry).map { case (v, c) => !v || c }).asUInt().andR()

  // Whether there is valid but not-carried entry exist
  val hasValidNoCarry: Bool =
    VecInit(strEntryValids.zip(strCreateIDCarry).map { case (v, c) => v && !c }).asUInt().orR() ||
      (syncQueue.io.deq.valid && !oldestWaitBits.strCreateID.asBools().last)

  // Whether there is valid and carried entry exist
  val hasValidAndCarry: Bool =
    VecInit(strEntryValids.zip(strCreateIDCarry).map { case (v, c) => v && c }).asUInt().orR() ||
      (syncQueue.io.deq.valid && oldestWaitBits.strCreateID.asBools().last)

  // The carry bit for new stream entry will be true, if there is non-carry entry and counter just wrapped
  // OR the carried entries and no-carried entries exist at the same time
  val newStrCarry: Bool =
    !carryReset && (hasValidNoCarry && strCreateIDBaseCtr.value === 0.U) || (hasValidNoCarry && hasValidAndCarry)

  // Calculate the minimum stream create ID
  require(strCreateID.nonEmpty, s"Command Queue should not be empty")
  val minStrCreateID: UInt = validReduceMin(strEntryValids.zip(strCreateID))._2 // Only the actual valid is needed

  // Calculate the bit that indicate this entry is valid, create ID is minimum, and ready to dispatcher
  // This ready bit does not consider conflict on flight
  val dispatchReadyBitRaw: Seq[Bool] =
    strEntryValids.zip(strCreateID).zip(queueStrEntryAvails).map { case ((valid, strCID), ready) =>
      // What stream entry is ready?
      // 1. Valid
      // 2. First stream command -- minimum stream command
      // 3. ports required are ready
      // 4. not blocked by valid sync command
      valid && strCID === minStrCreateID && ready && (!syncQueue.io.deq.valid || strCID <= oldestWaitBits.strCreateID)
    }

  // The true ready bit consider the conflict on-flight
  val dispatchReadyBit: Seq[Bool] =
    dispatchReadyBitRaw.zip(strRdyCtrs).map { case (b, ctr) => b && ctr.value === ctrlParam.dispStage.U }

  // Whether or not there is available entry in stream entry queue that is ready to dispatch
  val dispatchFromQueue: Bool = VecInit(dispatchReadyBit).asUInt().orR()

  // Whether new stream command can be dispatched directly
  val canBDispDirectly: Bool = !syncBlocking && !newStrEntryPortBusy && !dispatchFromQueue && queuyIsEmpty

  // Whether or not a valid new stream entry will be put into queue
  // 1. dispatching is blocked
  // 2. new stream entry's related port are taken
  // 3. there are valid entry in queue whose ports are not busy
  val newStrEnqueue: Bool = newStrEntry.valid && !canBDispDirectly

  // Whether or not a valid new stream entry will be dispatched directly from RoCC without enter queue
  val newStrDispatched: Bool = newStrEntry.valid && canBDispDirectly

  // Calculate the location of entry that will be dispatched from queue
  val locationDispatch: UInt = OHToUInt(dispatchReadyBit)

  // Stream Entry Dispatched From Queue
  val strEntryFromQueue: StreamDispatchBus = WireInit(0.U.asTypeOf(new StreamDispatchBus(dsa)))

  // Compute System is Busy
  val compBusy: Bool = VecInit(compStatuses.map(_.busy)).asUInt().orR()

  // Memory System is Busy
  val memBusy: Bool = VecInit(memsStatus.map(_.alive)).asUInt().orR()

  // Control System is Busy, entry in command queue or sync queue
  val ctrlBusy: Bool = VecInit(strEntryValids).asUInt().orR() || syncQueue.io.deq.valid

  // Start reconfiguring
  val startReconfiguring: Bool =
    isParaCfg && ssParaCfg.dsaReg1 === dsaRegCSA.id.U && ssParaCfg.dsaReg2 === dsaRegCFS.id.U

  // Reconfigurable node is being reconfigured
  val reconfiguring: Bool = VecInit(
    compStatuses.map(_.configured) ++ ivpsStatus.map(_.configured) ++ ovpsStatus.map(_.configured)
  ).asUInt().orR()

  // Decoded CPU rs1 Value for Synchronization Command
  val waitDecRs1Val: SyncEntry = Cat(cpuRs1Val, roccCmd.bits.inst.rd, Cat(newStrCarry, strCreateIDBaseCtr.value))
    .asTypeOf(new SyncEntry(ctrlParam))
  require(waitDecRs1Val.getWidth == (16 + 1 + 5 + strCreateID.head.getWidth), s"Format of sync entry is not correct")

  // All valid stream command comes before wait command (retire in order)
  require(strCreateID.length == strEntryValids.length)
  val allStrRetiredBeforeWait: Bool = VecInit(strCreateID.zip(strEntryValids).map { case (strID, valid) =>
    require(strID.getWidth == oldestWaitBits.strCreateID.getWidth)
    // Not valid OR valid command comes after wait
    !valid || strID > oldestWaitBits.strCreateID
  }).asUInt().andR()

  /* ------------------------- Combination Logic            ------------------------- */

  /* --- Connect all related fields to new stream entry --- */

  // valid
  newStrEntry.valid := isLinStr || isIndStr || isRecStr || isRecv
  // Target local port Id
  newStrEntry.targetLocalPortId match {
    case Some(value) =>
      value := Mux(
        isLinStr || isIndStr,
        linIndDecRs1Val.targetLocalPortId,
        Mux(isRecStr, recDecRs1Val.outputVPortId, Mux(isRecv, ssRecv.targetLocalPortId, 0.U))
      )
    case None =>
  }
  // Stream MStatus for DMA access, TLB need it
  newStrEntry.mStatus match {
    case Some(status) => status := roccCmd.bits.status
    case None         =>
  }
  // The data type of memory stream
  newStrEntry.memDataTypeExp match {
    case Some(value) =>
      value := memDataTypeExp
      require(
        value.getWidth == memDataTypeExp.getWidth,
        s"Number of bit that specify the stream data type is different, " +
          s"${value.getWidth} != ${memDataTypeExp.getWidth}"
      )
    case None =>
  }
  // The data type of constant stream (for generate engine)
  newStrEntry.constDataTypeExp match {
    case Some(value) =>
      value := constDataTypeExp
      require(
        value.getWidth == constDataTypeExp.getWidth,
        s"Number of bit that specify the const data type is different, " +
          s"${value.getWidth} != ${constDataTypeExp.getWidth}"
      )
    case None =>
  }
  // The memory engine that it goes to
  newStrEntry.memType :=
    Mux(
      (isLinStr && !linIndDecRs1Val.isGEN) || isIndStr,
      linIndDecRs1Val.memType,
      Mux(
        isRecStr,
        recMemType,
        Mux(isLinStr && linIndDecRs1Val.isGEN, genMemType, Mux(isRecv && ssRecv.isDiscard, disMemType, regMemType))
      )
    )
  // Memory Operation, only DMA, SPM and REG; REC, DIS and GEN ignore this field
  newStrEntry.memOperation :=
    Mux(
      isLinStr || isIndStr,
      linIndDecRs1Val.memOperation,
      Mux(isRecv, Mux(ssRecv.cpu2ivp, memOpRead, Mux(ssRecv.ovp2cpu, memOpWrite, 0.U)), 0.U)
    )
  // Stream Pattern Type: Linear or Indirect
  newStrEntry.LinOrInd match {
    case Some(value) => value := isIndStr
    case None        =>
  }
  // Start Point of the Stream
  newStrEntry.startPoint :=
    Mux(isRecv && ssRecv.cpu2ivp, cpuRs1Val, DSARegFile(dsaRegStreamStartAddr.id).get)
  // Stride 1D if exists
  assignRegVal2EntryField(newStrEntry.stride1D, DSARegFile(dsaRegStreamStride1D.id))
  // Number of linear dimension
  newStrEntry.numLinDim match {
    case Some(value) => value := linIndDecRs1Val.dimension
    case None        =>
  }
  // Linear Padding Mode
  newStrEntry.linearPadding match {
    case Some(value) => value := linIndDecRs1Val.streamMode
    case None        =>
  }
  // Recurrence the other port
  newStrEntry.recIVPortId match {
    case Some(value) => value := recDecRs1Val.inputVPortId
    case None        =>
  }
  // Length 1D
  assignRegVal2EntryField(newStrEntry.initLength1D, DSARegFile(dsaRegStreamLength1D.id))
  // Stride 2D
  assignRegVal2EntryField(newStrEntry.stride2D, DSARegFile(dsaRegStreamStride2D.id))
  // Stretch 2D
  assignRegVal2EntryField(newStrEntry.stretch2D, DSARegFile(dsaRegStreamStretch2D.id))
  // Length 2D
  assignRegVal2EntryField(newStrEntry.initLength2D, DSARegFile(dsaRegStreamLength2D.id))
  // Stretch 3D to 2D
  assignRegVal2EntryField(newStrEntry.stretch3D2D, DSARegFile(dsaRegStreamStretch3D2D.id))
  // Stretch 3D to 1D
  assignRegVal2EntryField(newStrEntry.stretch3D1D, DSARegFile(dsaRegStreamStretch3D1D.id))
  // Delta Stride 2D
  assignRegVal2EntryField(newStrEntry.deltaStride2D, DSARegFile(dsaRegStreamDeltaStride2D.id))
  // Delta Stretch 2D
  assignRegVal2EntryField(newStrEntry.deltaStretch2D, DSARegFile(dsaRegStreamDeltaStretch2D.id))
  // Stride 3D
  assignRegVal2EntryField(newStrEntry.stride3D, DSARegFile(dsaRegStreamStride3D.id))
  // Length 3D
  assignRegVal2EntryField(newStrEntry.initLength3D, DSARegFile(dsaRegStreamLength3D.id))
  // Indirect Index Stream, pass through only when it is indirect stream
  newStrEntry.indirectIdxStream match {
    case Some(value) => value := Mux(isIndStr, linIndDecRs1Val.indirectIndex, false.B)
    case None        =>
  }
  // Indirect Stride 2D stream, pass through only when it is indirect stream
  newStrEntry.indirectS2DStream match {
    case Some(value) => value := Mux(isIndStr, linIndDecRs1Val.indirectStride2D, false.B)
    case None        =>
  }
  // Indirect Length 1D stream, pass through only when it is indirect stream
  newStrEntry.indirectL1DStream match {
    case Some(value) => value := Mux(isIndStr, linIndDecRs1Val.indirectLength1D, false.B)
    case None        =>
  }
  // Number of Indirect Stream Pattern Dimension
  newStrEntry.numIndirectDim match {
    case Some(value) => value := linIndDecRs1Val.dimension
    case None        =>
  }
  // Index Stream Port Id
  assignRegVal2EntryField(newStrEntry.indexOVPortId, indexPortId, checkWidth = false)
  // Indirect Stride 2D Port Id
  assignRegVal2EntryField(newStrEntry.stride2DOVPortId, stride2DPortId, checkWidth = false)
  // Indirect Length 1D Port Id
  assignRegVal2EntryField(newStrEntry.length1DOVPortId, length1DPortId, checkWidth = false)
  // Index Stream Data Type
  assignRegVal2EntryField(newStrEntry.idxStrDataType, Some(idxStrDataTypeExp))
  // Indirect Stride2D Stream Data Type
  assignRegVal2EntryField(newStrEntry.s2dStrDataType, Some(s2dStrDataTypeExp))
  // Indirect Length1D Stream Data Type
  assignRegVal2EntryField(newStrEntry.l1dStrDataType, Some(l1dStrDataTypeExp))
  // Destination Register Number for Register Engine
  newStrEntry.rd match {
    case Some(rd) => rd := roccCmd.bits.inst.rd
    case None     =>
  }

  /* --- Enqueue the synchronization entry to Synchronization queue --- */

  // connect the valid
  syncQueue.io.enq.valid := isWait
  syncQueue.io.enq.bits := waitDecRs1Val

  /* --- Dequeue the synchronization entry and checking whether the oldest wait command is blocking */

  // Dequeue wait command based on :
  // 1. Oldest wait command is valid
  // 2. This wait command is not blocking anything
  // 3. Register response port is ready to receive signal value
  // 4. All previous command retired
  syncQueue.io.deq.ready := syncQueue.io.deq.valid && !syncBlocking && roccResp.ready && allStrRetiredBeforeWait

  // check whether the oldest wait command is blocking
  syncBlocking := syncQueue.io.deq.valid && {
    // Create bit blocking wire
    val waitBitsBlocking: Bool = WireInit(false.B)
    // Check all memory statuses to see if anyone of them are blocked
    val memBlocks: Seq[Bool] = memsStatus.zip(dsa.allMemNodeParam).map { case (memStatus, memParam) =>
      memParam.nodeType match {
        case DirectMemoryAccess =>
          oldestWaitBits.waitDMA && memStatus.alive && isBlockMemOp(oldestWaitBits.memOpMask, memStatus.memOpMask)
        case ScratchpadMemory =>
          oldestWaitBits.waitSPM && memStatus.alive && isBlockMemOp(oldestWaitBits.memOpMask, memStatus.memOpMask)
        case RecurrenceEngine =>
          oldestWaitBits.waitREC && memStatus.alive && isBlockMemOp(oldestWaitBits.memOpMask, memStatus.memOpMask)
        case DiscardEngine =>
          oldestWaitBits.waitDIS && memStatus.alive && isBlockMemOp(oldestWaitBits.memOpMask, memStatus.memOpMask)
        case GenerateEngine =>
          oldestWaitBits.waitGEN && memStatus.alive && isBlockMemOp(oldestWaitBits.memOpMask, memStatus.memOpMask)
        case RegisterEngine =>
          oldestWaitBits.waitREG && memStatus.alive && isBlockMemOp(oldestWaitBits.memOpMask, memStatus.memOpMask)
        case errType: Any =>
          require(requirement = false, s"Node Type $errType is not allowed")
          false.B
      }
    }
    // Check whether it is waiting for compute system to finish
    val compBlock: Bool = compBusy && oldestWaitBits.waitCompFinish
    // Assign to the bits Blocking wire
    waitBitsBlocking := VecInit(memBlocks).asUInt().orR() || compBlock
    // Return
    waitBitsBlocking
  }

  // Connect to stream dispatcher status wire
  dispStatus.reloadBits := reloadBitstream
  dispStatus.configuringBits := reconfiguring
  dispStatus.loadLastBits := configCtr.value =/= 0.U
  dispStatus.hasValidStrEntry := VecInit(strEntryValids).asUInt().orR()
  dispStatus.fullValidStrEntry := VecInit(strEntryValids).asUInt().andR()
  dispStatus.noValidStrEntry := !dispStatus.hasValidStrEntry
  dispStatus.hasValidSyncEntry := syncQueue.io.deq.valid
  dispStatus.fullValidSyncEntry := !syncQueue.io.enq.ready
  dispStatus.noValidSyncEntry := !dispStatus.hasValidSyncEntry

  /* ------------------------- Finite State Machine         ------------------------- */

  // Configure the register file of DSAGEN
  when(isParaCfg) {
    // Update the regular register
    writeReg(ssParaCfg.dsaReg1, cpuRs1Val, ssParaCfg.dsaReg1isStickiness, isParaCfg)
    writeReg(ssParaCfg.dsaReg2, cpuRs2Val, ssParaCfg.dsaReg2isStickiness, isParaCfg)
    // Update CSA and CFS will trigger bitstream reloading
    when(startReconfiguring) {
      reloadBitstream := true.B
      reqConfCtr.reset()
      rspConfCtr.reset()
    }
  }.elsewhen(!reloadBitstream && (isLinStr || isIndStr || isRecStr || isRecv) && roccCmd.fire()) {
    for (dsaRegIdx <- 0 until maxNumDSARegister) {
      // Get the register and stickiness
      val reg:        Option[UInt] = DSARegFile(dsaRegIdx)
      val stickiness: Option[Bool] = DSARegFileStickiness(dsaRegIdx)
      (reg, stickiness) match {
        // If register is not sticky, reset to zero when there is stream instantiation instruction
        case (Some(r), Some(s)) =>
          when(!s) {
            r := 0.U
          }
        // Not defined, do nothing
        case (None, None) =>
        case _ =>
          require(
            requirement = false,
            s"Existence of register and stickiness is problematic: " +
              s"register = ${reg.isDefined}, stickiness = ${stickiness.isDefined}"
          )
      }
    }
  }

  // Loading the Config Bitstream, kick start FSM
  when(reloadBitstream) {

    // Request to L1D if number of requested is not finished
    // TODO: We limited the number of pending bitstream request to be less than magic number, we should know better
    //  about the HellaCacheIO
    when(reqConfCtr.value =/= DSARegFile(dsaRegCFS.id).get && (reqConfCtr.value - rspConfCtr.value) < 1.U) {
      // Do request based on previous ready state, since ready can not be coupled with valid
      roccMem.req.valid := true.B
      // Increase counter and update request address if request is accepted
      when(roccMem.req.fire()) {
        // Increase the number of requested
        reqConfCtr.inc()
        // Increase to the next address to request
        DSARegFile(dsaRegCSA.id).get := DSARegFile(dsaRegCSA.id).get + (XLEN / 8).U
      }
    }.otherwise {
      // Requested all we need or request too much, stop requesting
      roccMem.req.valid := false.B
    }

    // Get the response from L1D
    when(
      roccMem.resp.valid &&
        rspConfCtr.value =/= DSARegFile(dsaRegCFS.id).get
    ) {
      // Increase the counter
      rspConfCtr.inc()
      // Put the response data into config register
      reconfPorts.foreach { reconfPort =>
        reconfPort := roccMem.resp.bits.data.asTypeOf(new ReconfPort(reconfParams.head))
        reconfPort.reset := false.B
        reconfPort.valid := true.B
      }
    }.otherwise {
      reconfPorts.foreach { reconfPort => reconfPort := 0.U.asTypeOf(new ReconfPort(reconfParams.head)) }
    }

    // Loading Stream finished
    when(reqConfCtr.value === DSARegFile(dsaRegCFS.id).get && rspConfCtr.value === DSARegFile(dsaRegCFS.id).get) {
      // Reconfiguring finished, the reason that configCtr.inc() cannot be put in upper when context is that
      // .inc() can only be called when requesting and responding of bitstream is finished
      // because .inc() IS finite state machine itself
      when(!reconfiguring && configCtr.inc()) {
        reloadBitstream := false.B
        reqConfCtr.reset()
        rspConfCtr.reset()
        configCtr.reset()
      }
    }
  }.elsewhen(startReconfiguring) {
    // reset all reconfigurable nodes
    reconfPorts.foreach { reconfPort =>
      reconfPort := 0.U.asTypeOf(new ReconfPort(reconfParams.head))
      reconfPort.valid := true.B
      reconfPort.reset := true.B
    }
  }.otherwise {
    // If we are not loading bit stream
    reconfPorts.foreach { reconfPort => reconfPort := 0.U.asTypeOf(new ReconfPort(reconfParams.head)) }
    roccMem.req.valid := false.B
  }

  // Put the newly created stream entry in the stream entry queue
  for (cmdIdx <- 0 until ctrlParam.dispQueueDepth) {
    when(newStrEnqueue && roccCmd.fire() && cmdIdx.U === locationNewStrEntry) {
      // Put the new stream entry into queue
      strEntryQueue(cmdIdx) := newStrEntry
      // Assign the new stream create ID base from counter and increase the counter
      strCreateIDBase(cmdIdx) := strCreateIDBaseCtr.value
      // Assign the carry bit
      strCreateIDCarry(cmdIdx) := newStrCarry
    }.elsewhen(carryReset) {
      strCreateIDCarry(cmdIdx) := false.B
    }
  }

  // Increase when there is an entry (stream entry: lin / ind / rec / reg, wait entry : wait) created
  when((newStrEnqueue && roccCmd.fire()) || isWait) {
    strCreateIDBaseCtr.inc()
  }

  // Dequeue the stream entry based on related port busy and its command ID
  when(dispatchFromQueue) {
    // Assign the Stream Entry from Queue wire by queue
    strEntryFromQueue := strEntryQueue(locationDispatch)
  }

  // Update the Stream Entry Valid
  strEntryValids.zipWithIndex.foreach { case (valid, idx) =>
    // Calculate enqueue/dequeue time
    val doEnqueue: Bool = !valid && newStrEnqueue && idx.U === locationNewStrEntry
    val doDequeue: Bool = valid && dispatchFromQueue && idx.U === locationDispatch
    // Update the valid register
    valid := Mux(
      doEnqueue,
      true.B, // Set the valid bit of new stream entry
      Mux(
        doDequeue,
        false.B, // Reset the valid bit of dispatched stream entry
        valid // Remain same
      )
    )
  }

  // Count the cycle that entry in queue is ready
  require(dispatchReadyBitRaw.length == strRdyCtrs.length)
  dispatchReadyBitRaw.zip(strRdyCtrs).foreach { case (ready, counter) =>
    when(ready && counter.value =/= ctrlParam.dispStage.U) {
      counter.inc()
    }.elsewhen(!ready) {
      counter.reset()
    }
  }

  // Connect the node statuses port to monitor status port
  dsaMonitor match {
    case Some(monitor) =>
      // Connect port from compute nodes
      require(
        monitor.compStatuses.length == compStatuses.length,
        s"${monitor.compStatuses.length} compute status port in monitor, ${compStatuses.length} port from nodes"
      )
      monitor.compStatuses.zip(compStatuses).foreach { case (mPort, nPort) => mPort := nPort }
      // Connect ports from memory nodes
      require(
        monitor.memStatuses.length == memsStatus.length,
        s"${monitor.memStatuses.length} memory status port in monitor, ${memsStatus.length} ports from nodes"
      )
      monitor.memStatuses.zip(memsStatus).foreach { case (mPort, nPort) => mPort := nPort }
      // Connect ports from sync nodes, first IVP then OVP
      require(
        monitor.syncStatuses.length == (ivpsStatus ++ ovpsStatus).length,
        s"${monitor.syncStatuses.length} sync status port in monitor, ${(ivpsStatus ++ ovpsStatus).length} ports from nodes"
      )
      monitor.syncStatuses.zip(ivpsStatus ++ ovpsStatus).foreach { case (mPort, nPort) => mPort := nPort }
      // Connect stream dispatcher
      monitor.ctrlStatus := dispStatus
      // Connect the trigger port of monitor
      monitor.recordStat.valid := isStat
      monitor.recordStat.bits := statIdx
    case None =>
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Bitstreamn loading from roccMem, except handshake signal
  roccMem.req.bits.cmd := M_XRD
  // Request address
  roccMem.req.bits.addr := DSARegFile(dsaRegCSA.id).get
  // Maximum size (0=1-byte, 1=2-byte, 2=4-byte, 3=8-byte)
  roccMem.req.bits.size := "b11".U
  require(roccMem.req.bits.size.getWidth == 2, s"make sure 0=1-byte, 1=2-byte, 2=4-byte, 3=8-byte?")

  // Update the ivp setting register
  for (ivpIdx <- 0 until dsa.numIVP) {
    ivpSetPorts(ivpIdx).vpRegIdx := ssPortCfg.vpSetRegIdx
    ivpSetPorts(ivpIdx).vpRegVal := cpuRs1Val
    // When ivp index matched
    when(
      roccCmd.fire() &&
        isPortCfg && ssPortCfg.isIVP && ivpIdx.U(MAX_LOCAL_VPORT_ID_BIT.W) === ssPortCfg.targetVPIdx
    ) {
      ivpSetPorts(ivpIdx).valid := true.B
      ivpSetPorts(ivpIdx).reset := false.B
    }.elsewhen(startReconfiguring) {
      ivpSetPorts(ivpIdx).valid := true.B
      ivpSetPorts(ivpIdx).reset := true.B
    }.otherwise {
      ivpSetPorts(ivpIdx).valid := false.B
      ivpSetPorts(ivpIdx).reset := false.B
    }
  }

  // TODO: Configure Output Vector Port, Task Flow discussion

  // RoCC Command Ready Signal
  // SSPortCfg and SSParaCfg are always ready, except when reloading bitstream
  // LinStrm, IndStrm, RecStrm, Recv: if this new entry needs to go into queue, then check queue fullness
  // Wait: check the sync queue fullness
  // Port Config: ready if queue if empty
  roccCmd.ready :=
    !reloadBitstream && Mux(
      newStrEnqueue && (isLinStr || isIndStr || isRecStr || isRecv),
      !strQueueFull,
      Mux(isWait, syncQueue.io.enq.ready, Mux(isPortCfg, queuyIsEmpty, true.B))
    )

  // Dispatch: Choose between dispatch directly or dispatch from queue
  strDispPort := Mux(
    dispatchFromQueue,
    strEntryFromQueue,
    Mux(newStrDispatched, newStrEntry, 0.U.asTypeOf(new StreamDispatchBus(dsa)))
  )

  // RoCC busy, compute / memory / ctrl is busy, two cycles is DSA command received
  roccBusy := compBusy || memBusy || ctrlBusy || isDSA

  // Output to RoCC response when barrier is dequeued or statistic result is collected
  // Statistic collection has higher priority
  val (statValid, stat): (Bool, UInt) = {
    //
    if (dsaMonitor.isEmpty) (RegNext(isStat), Fill(roccResp.bits.data.getWidth, 1.U(1.W)).asUInt())
    else (dsaMonitor.get.statistic.valid, dsaMonitor.get.statistic.bits)
  }
  roccResp.valid := syncQueue.io.deq.fire() || statValid
  roccResp.bits.rd := Mux(statValid, RegNext(roccCmd.bits.inst.rd), syncQueue.io.deq.bits.rd)
  roccResp.bits.data := Mux(statValid, stat, syncQueue.io.deq.bits.rdVal)

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  // Instruction Type can never be more than one type
  val instTypeOH: UInt = VecInit(isLinStr, isIndStr, isRecStr, isParaCfg, isPortCfg, isWait, isRecv).asUInt()
  assert(PopCount(instTypeOH) <= 1.U, s"SS instruction can only be one type at a time")

  // Exact match wait command should not be illegal
  assert(!(oldestWaitBits.illegal && syncQueue.io.deq.valid), s"Wait command entry is illegal")

  /* ------------------------- Utility                      ------------------------- */

  // Assign the possible register value to entry field
  def assignRegVal2EntryField(entryField: Option[UInt], regVal: Option[UInt], checkWidth: Boolean = true): Unit = {
    (entryField, regVal) match {
      case (Some(field), Some(value)) =>
        if (checkWidth)
          require(
            field.getWidth == value.getWidth,
            s"Field ${field.name} Bit width mismatch, ${field.getWidth} != ${value.getWidth}"
          )
        field := value
      case (None, None)    => // both does not exist, nothing happen
      case (None, Some(_)) => // Stream entry does not need it but register has, it is normal
      case (Some(_), None) => // Wrong
        require(requirement = false, s"Field and Reg existence is problematic, $entryField, $regVal")
    }
  }

  // Check whether the synchronization command is blocking the memory operation of memory status
  def isBlockMemOp(waitMemOpMask: UInt, memStatusOpMask: UInt): Bool = {
    require(
      waitMemOpMask.getWidth == memStatusOpMask.getWidth,
      s"Memory Op Mask is not the same width, ${waitMemOpMask.getWidth} != ${memStatusOpMask.getWidth}"
    )
    (waitMemOpMask & memStatusOpMask).orR()
  }

  // Check whether the related port of full stream entry is busy
  def streamPortsBusy(
    // Stream Entry (Bus)
    strEntry: StreamDispatchBus,
    // Vector Port Status Check, existing, dispatched, active stream
    ivpsStatus: Vec[VectorPortStatus],
    ovpsStatus: Vec[VectorPortStatus],
    // Future port hold check, entry in queue
    queueEntries: Seq[StreamDispatchBus] = Nil,
    queueValids:  Seq[Bool] = Nil,
    queueCheck:   Boolean = false
  ): Bool = {
    // The Target Port is Busy, judge between memory operation
    val targetPortBusy: Bool =
      Mux(
        strEntry.memOperation === memOpRead,
        ivpsStatus(strEntry.targetLocalPortId.getOrElse(0.U)).alive,
        ovpsStatus(strEntry.targetLocalPortId.getOrElse(0.U)).alive
      )
    // Indirect Related Ports are busy
    val indirectPortsBusy: Bool = {
      // Index Stream Port Busy
      val indexPortBusy: Bool = strEntry.indexOVPortId match {
        case Some(ovpId) => ovpsStatus(ovpId).alive && strEntry.indirectIdxStream.getOrElse(dsa.supportIndirectIdx.B)
        case None        => false.B
      }
      // Stride 2D Stream Port Busy
      val s2dPortBusy: Bool = strEntry.stride2DOVPortId match {
        case Some(ovpId) => ovpsStatus(ovpId).alive && strEntry.indirectS2DStream.getOrElse(dsa.supportIndirectS2D.B)
        case None        => false.B
      }
      // Length 1D Stream Port
      val l1dPortBusy: Bool = strEntry.length1DOVPortId match {
        case Some(ovpId) => ovpsStatus(ovpId).alive && strEntry.indirectL1DStream.getOrElse(dsa.supportIndirectL1D.B)
        case None        => false.B
      }
      // Return
      indexPortBusy || s2dPortBusy || l1dPortBusy
    }
    // Check whether possible DMA Linear is busy
    val dmaLinearBusy: Bool = // Linear
      strEntry.memType === dmaMemType && strEntry.isLinear && targetPortBusy
    val dmaIndirectBusy: Bool = // Indirect
      strEntry.memType === dmaMemType && strEntry.isIndirect && (targetPortBusy || indirectPortsBusy)

    // Check whether possible SPM Stream is busy
    val spmLinearBusy: Bool = // Linear
      strEntry.memType === spmMemType && strEntry.isLinear && targetPortBusy
    val spmIndirectBusy: Bool = // Indirect
      strEntry.memType === spmMemType && strEntry.isIndirect && (targetPortBusy || indirectPortsBusy)

    // Check whether possible GEN Linear Stream is Busy
    val genLinearBusy: Bool = // Linear
      strEntry.memType === genMemType && strEntry.isLinear && targetPortBusy
    val genIndirectBusy: Bool = // Indirect
      strEntry.memType === genMemType && strEntry.isIndirect && (strEntry.isLinear && targetPortBusy)

    // Check whether possible REC Stream is Busy, recurrence should always check Input and Output ports
    val recBusy: Bool =
      strEntry.memType === recMemType &&
        (ivpsStatus(strEntry.recIVPortId.getOrElse(0.U)).alive || ovpsStatus(strEntry.recOVPortId).alive)

    // Check whether possible DIS Linear Stream is Busy (
    val disBusy: Bool = strEntry.memType === disMemType && targetPortBusy

    // Check whether possible REG Scalar Stream is Busy
    val regBusy: Bool = strEntry.memType === regMemType && targetPortBusy

    // Merge all busy signal
    val portBusy: Bool = (dmaLinearBusy || dmaIndirectBusy) || (spmLinearBusy || spmIndirectBusy) ||
      (genLinearBusy || genIndirectBusy) || recBusy || disBusy || regBusy

    // Whether queue stream entry is stopping stream entry, for now only consider recurrence stream
    val queueBusy: Bool = if (queueCheck) {
      // Check the length of valid and stream entry is matched
      require(queueEntries.length == queueValids.length)
      // Check busy (conflict state) for each stream entry of queue
      val busyBits: Seq[Bool] = queueEntries.zip(queueValids).map { case (queueEntry, queueValid) =>
        // Conflict with recurrence stream entry input vector port
        val recIVPConflict: Bool = strEntry.memOperation === memOpRead && // Is read AND
          queueEntry.recIVPortId.getOrElse(0.U) === strEntry.targetLocalPortId.getOrElse(0.U) // IVP conflict
        // Conflict with recurrence stream entry output vector port
        val recOVPConflict: Bool = strEntry.memOperation =/= memOpRead && // Not read AND
          queueEntry.recOVPortId === strEntry.targetLocalPortId.getOrElse(0.U)
        // TODO: please be attention, we do not check indirect port conflict, this is A KNOWN error
        // the reason is just I am too lazy and no case shows we need it
        val recOVPIndConflict: Bool = false.B
        // The entry in queue should be valid
        queueValid && queueEntry.memType === recMemType && (recIVPConflict || recOVPConflict || recOVPIndConflict)
      }
      // OR reduce
      VecInit(busyBits).asUInt().orR()
    } else {
      // If check is not needed, then both queue entry and valid should be empty
      require(queueEntries.isEmpty && queueValids.isEmpty)
      // No queue check is needed, always false
      false.B
    }

    // Return
    strEntry.valid && (portBusy || queueBusy)
  }

  /* ------------------------- Post Generation Sanity Check ------------------------- */
}
