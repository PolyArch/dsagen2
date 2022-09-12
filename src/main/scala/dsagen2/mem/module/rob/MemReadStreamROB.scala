package dsagen2.mem.module.rob

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.mem.module.bank.SinglePortSyncReadRAM
import dsagen2.top.config.DSAFixedConfig.{memOpAtomOp5, memOpRead, MAX_VP_BYTE_BITS}
import dsagen2.util.RegUtil.RegNextN
import dsagen2.util.StreamUtil.strReq2memReq
import dsagen2.util.UIntUtil.groupBitsAs

/** Memory Stream Response Reorder Buffer, only read request goes to this buffer. Write and atomic operation order
  * is managed by TileLink and scratchpad itself
  *
  * @param memNode   Memory Node Parameters
  * @param ivpsParam IVP Parameters
  * @param ovpsParam OVP Parameters
  * @param p         CDE
  */
class MemReadStreamROB(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit p: Parameters)
    extends MultiIOModule {
  /* -------------------------      Extract Parameters      ------------------------- */

  // Size of ROB
  val robSize: Int = memNode.numPendingRequest

  // Read Latency for the ROB
  val robReadLatency: Int = 2

  /* -------------------------     Derived Parameters       ------------------------- */

  // Number of bits needed for numEntry
  def entryIdxBits: Int = log2Ceil(robSize)

  /* -------------------------      Parameters Sanity Check ------------------------- */

  // Sanity Check on number of entry
  require(isPow2(robSize) && robSize >= 2, s"Number of ROB entry should be power of 2, at least two entry needed")

  // Only Indirect Index Scratchpad and DMA needs ROB
  require(memNode.needROB, s"Memory Node $memNode seems not need ROB")

  // Depth needs to be positive
  require(memNode.numPendingRequest > 0)

  // ROB size should be larger than the number of IVP + 4, TODO: 4 is number of stage between stream select arb and ROB
  require(robSize > ivpsParam.length + 4, s"#IVP = ${ivpsParam.length}, size of ROB = $robSize")

  /* -------------------------         Input / Output       ------------------------- */

  // O, ROB active entry count
  val numValid: UInt = IO(Output(UInt(log2Ceil(robSize + 1).W)))

  // I, New Vector (deal with bank scratchpad) of Request port from AGU that need Reorder,
  // AGU also send it to Memory
  // For bank scratchpad, there will be multiple request port
  // valid can be different
  // meta should be same
  // state should be same
  // addr can be different in indirect index stream, should be same in linear stream
  // mask is expected mask
  // writeData is useless
  val strReqPorts: Vec[StreamRequest] = IO(
    Input(Vec(memNode.numMemReqPort, new StreamRequest(memNode, ivpsParam, ovpsParam)))
  )

  // O, Memory Request Ports to Memory (1 for DMA, N for banked scratchpad), just filter reqPorts
  val memReqPorts: Vec[MemRequest] = IO(Output(Vec(memNode.numMemReqPort, new MemRequest(memNode))))

  // I, Memory Response Ports from Memory (1 for DMA, N for banked scratchpad)
  val memRspPorts: Vec[MemResponse] = IO(Input(Vec(memNode.numMemReqPort, new MemResponse(memNode))))

  // O, Stream Response (if all part of reqPort collected) sent to Input Vector Port
  val strRspPort: StreamResponse = IO(Output(new StreamResponse(memNode, ivpsParam, ovpsParam)))

  // Input, Stream Response is accepted by Input Vector Port
  val strAccepted: Bool = IO(Input(Bool()))

  // O, ROB ID : Assigned ROB Id (The row index for newly request entry)
  val ROBId: UInt = IO(Output(UInt(entryIdxBits.W)))

  // Valid bitvector
  val robVlds: UInt = IO(Output(UInt(robSize.W)))

  // ROB Port ID
  val robPortIDs: Vec[UInt] = IO(Output(Vec(robSize, UInt(log2Ceil(ivpsParam.length).W))))

  // Capacity of each IVP in byte, max number of byte of for each IVP is 1KB for now
  val ivpsCapa: Vec[UInt] = IO(Input(Vec(ivpsParam.length, UInt(MAX_VP_BYTE_BITS.W))))

  // Current number of byte active for each IVP
  val ivpsLeftBytes: Vec[UInt] = IO(Input(Vec(ivpsParam.length, UInt(MAX_VP_BYTE_BITS.W))))

  /* -------------------------     Registers and Modules    ------------------------- */

  // F, Memory Block that holds the read data
  val dataBlocks: Seq[SinglePortSyncReadRAM] =
    if (memNode.isSPM && memNode.IndirectIndexStream && memNode.numSpmBank > 1) {
      Seq.fill(memNode.numSpmBank)(
        Module(
          new SinglePortSyncReadRAM(
            unitBits = memNode.memUnitBits,
            widthBits = memNode.memUnitBits * memNode.spmBankWidth,
            depth = robSize,
            maskWrite = true,
            latency = robReadLatency,
            isFPGA = p(FPGAOverlay)
          )
        )
      )
    } else {
      Seq.fill(1)(
        Module(
          new SinglePortSyncReadRAM(
            unitBits = memNode.memUnitBits,
            widthBits = memNode.memUnitBits * memNode.bandwidth,
            depth = robSize,
            maskWrite = true,
            latency = robReadLatency,
            isFPGA = p(FPGAOverlay)
          )
        )
      )
    }

  // F, Register Array that holds pending stream request, TODO: valid can be optimized out
  val robArray: Vec[StreamROBEntry] = RegInit(
    VecInit(Seq.fill(robSize)(0.U.asTypeOf(new StreamROBEntry(memNode, ivpsParam, ovpsParam))))
  )

  // F, Register Array that indicate whether entry is valid
  val robValids: Vec[Bool] = RegInit(VecInit(Seq.fill(robSize)(false.B)))

  // F, Register Array that store the stream order in ROB
  val robOrders: Vec[UInt] = RegInit(VecInit(Seq.fill(robSize)(0.U(entryIdxBits.W))))

  // F, Register Array that holds the mask indicate whether data is valid from memory response
  val maskArray: Vec[UInt] = RegInit(VecInit(Seq.fill(robSize)(0.U(memNode.bandwidth.W))))

  // Arbiter that round robin select between ready entry
  val deqArb: RRArbiter[Bool] = Module(new RRArbiter[Bool](Bool(), robSize))

  // Register of bit vector that indicate whether entry in ROB is in read pipeline
  val inReadPipe: Seq[Bool] = Seq.fill(robSize)(RegInit(false.B))

  // Counter for each IVP to keep track of the number of byte onflight
  val onflyByteCtrs: Vec[UInt] = RegInit(VecInit(Seq.fill(ivpsParam.length)(0.U(MAX_VP_BYTE_BITS.W))))

  /* -------------------------             Wires            ------------------------- */

  // ROB write address for all memory response port
  val robWriteAddrs: Seq[UInt] = memRspPorts.map(_.robId.getOrElse(0.U))

  // ROB write enable for all memory response ports
  val robWriteValids: Seq[Bool] = memRspPorts.map(_.isValidReadResp)

  // ROB read address, dequeue arbiter chosen
  val readAddr: UInt = deqArb.io.chosen

  // ROB read request, output valid of dequeue arbiter -- only means there is valid read request, not read enable
  val robReadValid: Bool = deqArb.io.out.valid

  // ROB read enable, A valid read request while write valids are all false
  val robReadEnable: Bool = !VecInit(robWriteValids).asUInt().orR() && robReadValid

  // C, request new means stream request is valid and operation is memOpRead
  val requestNew: Bool = VecInit(strReqPorts.map(r => r.meta.memOperation.get === memOpRead && r.valid)).asUInt().orR()

  // C, Meta Info of new request (they should all be same for multi request port, so we take the first one)
  val reqMeta: StreamMeta = strReqPorts.head.meta

  // Valid from request port, although request valid should be on at the same time since it is from
  // the same AGU and no inconsist should be generate from there. I still OR reduce them is good
  // but TODO: I still need to check whether they will be different
  val reqPortExistValid: Bool = VecInit(strReqPorts.map(_.valid)).asUInt().orR()

  // C, Indicate whether there is need to enqueue entry
  val doEnq: Bool =
    requestNew && reqPortExistValid && reqMeta.memOperation.getOrElse(memOpAtomOp5) === memOpRead

  // C, Stream State of new request (they should all be same for multi request port, so we take the first one)
  val newSteamState: Option[StreamState] = strReqPorts.head.state

  // C, Expected Mask of new Stream Request by concat all req ports
  val newExpectMask: UInt = VecInit(strReqPorts.map(_.mask.get)).asUInt()

  // C, Stream Order for new ROB entry
  val newOrder: UInt = WireInit(0.U(entryIdxBits.W))

  // Wire indicate the readiness of each entry to dequeue
  val deqReadiness: Vec[Bool] = WireInit(VecInit(Seq.fill(robSize)(false.B)))

  // Whether there is a valid read response from ROB RAM -- read valid delayed
  val doDeq: Bool = RegNextN(robReadEnable, robReadLatency).last

  // C, Position to dequeue entry from ROB
  val deqPosition: UInt = RegNextN(readAddr, robReadLatency).last

  // C, Availability to hold new entry (not just invert valid bit, but take dequeue into consideration)
  val entryEmpty: Seq[Bool] = robValids.slice(ivpsParam.length, robSize).map(!_)
  require(entryEmpty.length == (robSize - ivpsParam.length), s"Number of shared entry empty = ${entryEmpty.length}")

  // C, Position to place the new ROB entry
  val enqPosition: UInt =
    Mux(
      !robValids(reqMeta.targetLocalPortId.getOrElse(0.U)),
      reqMeta.targetLocalPortId.getOrElse(0.U),
      PriorityEncoder(entryEmpty) + ivpsParam.length.U
    ) //PriorityEncoder(entryEmpty)

  // Target Local Port ID (stream id) that being dequeue
  val deqPortID: UInt = robArray(readAddr).meta.targetLocalPortId.getOrElse(0.U)

  // Bit vector that indicate valid entry whose Port ID is the same new enqueue port id
  val matchValidNewPortId: Vec[Bool] = WireInit(VecInit(Seq.fill(robSize)(false.B)))

  // Bit vector that indicate valid, Port ID is the same with being dequeued Port ID
  val matchValidDeqPortId: Vec[Bool] = WireInit(VecInit(Seq.fill(robSize)(false.B)))

  // C, Bit vector that indicate whether current entry is being dequeued -- put into read pipe
  val beingDequeued: Seq[Bool] = for (entryIdx <- 0 until robSize) yield {
    robReadEnable && entryIdx.U === readAddr && !inReadPipe(entryIdx)
  }

  // C, bit vector that indicate whether current entry is being enqueued
  val beingEnqueued: Vec[Bool] = VecInit(
    for (entryIdx <- 0 until robSize) yield {
      entryIdx.U === enqPosition && doEnq
    }
  )

  // C, Bit indicate: valid, match new port id, not being dequeued
  val matchValidNewPortIdMatchNotDequeue: Seq[Bool] =
    matchValidNewPortId.zip(beingDequeued).map { case (match1, beingDeq) => match1 && !beingDeq }

  // Whether IVP satisfy capacity requirement for issue
  require(onflyByteCtrs.length == ivpsCapa.length && ivpsCapa.length == ivpsLeftBytes.length)
  val ivpsSpace: Vec[UInt] = VecInit(onflyByteCtrs.zip(ivpsCapa).zip(ivpsLeftBytes).map {
    case ((onfly, capa), currLeft) => Mux(currLeft > onfly, currLeft - onfly, 0.U)
  })

  // Calculate number of expected byte for each entry in ROB
  val expectByte4EachEntry: Vec[UInt] = VecInit(robArray.map { rob => PopCount(rob.expectMask) })

  // Calculate whether target IVP's space is allowed for enqueue
  val spaceAllow4EachEntry: Seq[Bool] = for (entryIdx <- 0 until robSize) yield {
    val ivpId: UInt = robArray(entryIdx).meta.targetLocalPortId.getOrElse(0.U)
    expectByte4EachEntry(entryIdx) <= ivpsSpace(ivpId)
  }

  // The IVP index that is going to enter read pipe
  val ivpEnq:     UInt = robArray(readAddr).meta.targetLocalPortId.getOrElse(0.U)
  val ivpEnqByte: UInt = expectByte4EachEntry(readAddr)

  // The IVP index that is coming out of read pipe
  val ivpDeq:     UInt = RegNextN(ivpEnq, robReadLatency).last
  val ivpDeqByte: UInt = RegNextN(ivpEnqByte, robReadLatency).last

  /* -------------------------     Combinational Logics     ------------------------- */

  // Judge whether the entry is ready for dequeue
  for (entryIdx <- 0 until robSize) {
    // Valid entry, First response for its port, not in the read pipe
    deqReadiness(entryIdx) :=
      // ROB Entry is valid
      robValids(entryIdx) && spaceAllow4EachEntry(entryIdx) &&
        RegNext(
          // ROB Entry is valid
          robValids(entryIdx) &&
            // first request of this local port
            robOrders(entryIdx) === 0.U &&
            // get all of the data unit, expected mask == current mask
            ((maskArray(entryIdx) & robArray(entryIdx).expectMask) === robArray(entryIdx).expectMask)
        ) && !inReadPipe(entryIdx) // not in the read request pipeline
  }

  // Check whether each entry match new entry port id and it is valid
  for (entryIdx <- 0 until robSize) {
    matchValidNewPortId(entryIdx) := robValids(entryIdx) &&
      robArray(entryIdx).meta.targetLocalPortId.getOrElse(0.U) === reqMeta.targetLocalPortId.getOrElse(0.U)
  }

  // Match: valid, port id match dequeued port id
  for (entryIdx <- 0 until robSize) {
    matchValidDeqPortId(entryIdx) := robValids(entryIdx) &&
      robArray(entryIdx).meta.targetLocalPortId.getOrElse(0.U) === deqPortID
  }

  // Connect new order for current ROB entry, new order is equal to maximum order of
  newOrder := PopCount(matchValidNewPortIdMatchNotDequeue)

  // Connect to the dequeue round robin arbiter
  deqArb.io.in.zip(deqReadiness).foreach { case (in, deqReadiness) =>
    in.bits := false.B // Bits does not matter
    // use the dequeue readiness as indication of selection
    // but the entry will only be dequeued when the ivp is ready -- stream response accepted
    in.valid := deqReadiness
  }
  // If there is no memory response (write to ROB), we can start dequeue from ROB -- doing read
  deqArb.io.out.ready := !VecInit(robWriteValids).asUInt().orR()

  // Connect write data and write enable (write mask) to block
  if (memNode.isSPM && memNode.IndirectIndexStream && memNode.numSpmBank > 1) {
    // Update data Array from memory response, update by mask, for Scratchpad
    require(memRspPorts.length == dataBlocks.length)
    robArray.forall { m: StreamROBEntry =>
      val widthMatch: Boolean = m.expectMask.getWidth == memNode.spmBankWidth * memNode.numSpmBank
      widthMatch
    }
    memRspPorts.zip(dataBlocks).zipWithIndex.foreach { case ((rspPort, block), bankIdx) =>
      // Group expected mask per bank
      val groupExpectMask: UInt =
        groupBitsAs(robArray(rspPort.robId.get).expectMask, memNode.spmBankWidth).apply(bankIdx)
      require(rspPort.mask.getWidth == groupExpectMask.getWidth)
      // Get write signal
      val writeData: UInt = rspPort.data
      val writeMask: UInt = rspPort.mask & groupExpectMask
      // Sanity check to make sure width match
      require((writeData.getWidth / memNode.memUnitBits) == memNode.spmBankWidth)
      require(writeMask.getWidth == memNode.spmBankWidth)
      // Connect write mask enable
      require(block.writeEnable.getWidth == writeMask.getWidth)
      block.writeEnable := Mux(rspPort.valid, writeMask, 0.U)
      // Connect write data
      require(block.writeData.getWidth == writeData.getWidth)
      block.writeData := writeData
    }
  } else {
    // Update the data sync memory block, for DMA and linear SPM or singel bank SPM
    require(dataBlocks.length == 1)
    require(memRspPorts.length == 1)
    val writeData: UInt = memRspPorts.head.data
    val writeMask: UInt = memRspPorts.head.mask & robArray(memRspPorts.head.robId.get).expectMask
    require(writeData.getWidth == memNode.bandBits)
    require(writeMask.getWidth == memNode.bandwidth)
    dataBlocks.head.writeData := writeData
    dataBlocks.head.writeEnable := Mux(memRspPorts.head.valid, writeMask, 0.U)
  }

  // Connect block enable (include read enable and write enable), together with address
  require(dataBlocks.length == robWriteAddrs.length)
  require(dataBlocks.length == robWriteValids.length)
  dataBlocks.zip(robWriteAddrs).zip(robWriteValids).foreach { case ((block, writeAddr), writeValid) =>
    // Enable block if there is write or read
    block.enable := writeValid || robReadValid
    // Write first
    block.address := Mux(writeValid, writeAddr, readAddr)
  }

  /* -------------------------     Finite State Machine     ------------------------- */

  // Enqueue: write the meta info array
  when(doEnq) {
    robArray(enqPosition).meta := reqMeta
    robArray(enqPosition).state match {
      case Some(value) => value := newSteamState.getOrElse(0.U.asTypeOf(new StreamState))
      case None        =>
    }
    robArray(enqPosition).expectMask := newExpectMask
  }

  // Enqueue: validate the valid array
  for (entryIdx <- 0 until robSize) {
    val curr: Bool = robValids(entryIdx)
    robValids(entryIdx) := Mux(
      beingEnqueued(entryIdx),
      true.B, // enqueue, turn it on
      Mux(
        beingDequeued(entryIdx),
        false.B, // being dequeued, turn it off
        curr
      ) // keep the same
    )
  }

  // Update the order array
  for (entryIdx <- 0 until robSize) {
    val curr: UInt = robOrders(entryIdx)
    // If: not being enqueued AND ROB doing dequeue AND not being dequeued itself AND dequeued port id is the same
    robOrders(entryIdx) := Mux(
      /* Decrease the stream order of entry when
       * This entry is not being put into read queue, some one else is being put into read queue,
       * entry has the same port with that of being put into read queue, and its order is not zero*/
      !beingEnqueued(entryIdx) && robReadEnable && matchValidDeqPortId(entryIdx) && curr =/= 0.U,
      /* Decrease stream order by one*/
      curr - 1.U,
      /* If this one is being enqueued, then replaced with new order, otherwise remain the current value*/
      Mux(beingEnqueued(entryIdx), newOrder, curr) // replace with new order
    )
  }

  // Update mask Array from memory response, do bitwise OR
  for (entryIdx <- 0 until robSize) {
    val curr: UInt = maskArray(entryIdx)
    // Split the curr value into #response group
    val currGroupByPort: Seq[UInt] =
      groupBitsAs(curr, curr.getWidth / memRspPorts.length)
    require(currGroupByPort.length == memNode.numMemReqPort)
    // Calculate the next mask by port
    val nextMaskByPort: Seq[UInt] = for (portIdx <- 0 until memNode.numMemReqPort) yield {
      val currPortMask: UInt = currGroupByPort(portIdx)
      val valid:        Bool = memRspPorts(portIdx).isValidReadResp
      val mask:         UInt = memRspPorts(portIdx).mask
      val robId:        UInt = memRspPorts(portIdx).robId.get
      require(currPortMask.getWidth == mask.getWidth)
      // Set the bit by using the response mask, bitwise OR
      Mux(valid && robId === entryIdx.U, currPortMask | mask, currPortMask)
    }
    // Concat to get next Data
    val updDataByMask: UInt = VecInit(nextMaskByPort).asUInt()
    // Update the collected data mask, when enqueue/dequeue happens, clear it
    when(beingEnqueued(entryIdx) || beingDequeued(entryIdx)) {
      maskArray(entryIdx) := 0.U
    }.otherwise {
      maskArray(entryIdx) := updDataByMask
    }
  }

  // Update read request in pipe state
  for (entryIdx <- 0 until robSize) {
    when(deqReadiness(entryIdx) && readAddr === entryIdx.U && robReadEnable) {
      inReadPipe(entryIdx) := true.B
    }.elsewhen(deqPosition === entryIdx.U && doDeq) {
      inReadPipe(entryIdx) := false.B
    }
  }

  // Update IVP on-flight number of byte counter
  for (ivpIdx <- ivpsParam.indices) {
    val currCount: UInt = onflyByteCtrs(ivpIdx)
    val ivpDoEnq:  Bool = robReadEnable && ivpIdx.U === ivpEnq
    val ivpDoDeq:  Bool = doDeq && ivpIdx.U === ivpDeq
    when(ivpDoDeq && ivpDoEnq) {
      onflyByteCtrs(ivpIdx) := currCount + ivpEnqByte - ivpDeqByte
    }.elsewhen(ivpDoEnq) {
      onflyByteCtrs(ivpIdx) := currCount + ivpEnqByte
    }.elsewhen(ivpDoDeq) {
      onflyByteCtrs(ivpIdx) := currCount - ivpDeqByte
    }
  }

  /* -------------------------       Output Connection      ------------------------- */

  // Report number of valid ROB Entry
  numValid := PopCount(robValids) +& 1.U

  // Filter the stream request port to memory request port
  require(strReqPorts.length == memReqPorts.length)
  strReqPorts.zip(memReqPorts).zipWithIndex.foreach { case ((strReq, memReq), reqIdx) =>
    // Connect stream request to
    strReq2memReq(strReq, memReq)
    memReq.robId.get := enqPosition
    memReq.reqIdx match {
      case Some(ri) =>
        // Switch between ascending vector request and descending vector request
        if (reqMeta.isDescend.isDefined) {
          // Calculate the descending request vector id
          val dReqIdx: Int = memNode.numMemReqPort - 1 - reqIdx
          ri := Mux(reqMeta.isDescend.get, dReqIdx.U, reqIdx.U)
        } else {
          // Only const stream or ascending stream, so just use request vector id as reqId for routing
          ri := reqIdx.U
        }
      case None =>
    }
  }

  // ROB ID
  ROBId := enqPosition

  // Dequeue from ROB and send it to stream response port
  strRspPort.valid := doDeq
  strRspPort.meta := RegNextN(robArray(readAddr).meta, robReadLatency).last
  strRspPort.state match {
    case Some(value) =>
      val state = RegNextN(robArray(readAddr).state.getOrElse(0.U.asTypeOf(new StreamState)), robReadLatency).last
      value := state
    case None =>
  }
  strRspPort.mask match {
    case Some(mask) =>
      require(mask.getWidth == robArray(readAddr).expectMask.getWidth)
      mask := RegNextN(robArray(readAddr).expectMask, robReadLatency).last
    case None =>
  }
  strRspPort.readData match {
    case Some(data) =>
      if (memNode.isSPM && memNode.IndirectIndexStream && memNode.numSpmBank > 1) {
        val readData: UInt = VecInit(dataBlocks.map { block =>
          val readDataPerBlock: UInt = block.readData
          require(readDataPerBlock.getWidth == memNode.spmBankWidth * memNode.memUnitBits)
          readDataPerBlock
        }).asUInt()
        require(data.getWidth == readData.getWidth)
        data := readData
      } else {
        require(dataBlocks.length == 1)
        val readDataPort: UInt = dataBlocks.head.readData
        require(data.getWidth == readDataPort.getWidth)
        data := readDataPort
      }
    case None =>
      require(requirement = false, "I dont think this will happen, existence should be judged on upper level")
  }

  // ROB Valids
  robVlds := robValids.asUInt()

  // ROB PortIDs
  robPortIDs.zip(robArray.map(_.meta.targetLocalPortId)).foreach { case (out, portId) => out := portId.getOrElse(0.U) }
}
