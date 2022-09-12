package dsagen2.mem.module.stab

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.mem.module.agu.OVPRequestMaskGen
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.util.{QueueFPGA, WithQueueIO}

/** Stream Table dedicated to Recurrence Engine
  */
class RecStreamTable(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit p: Parameters)
    extends MultiIOModule
    with BaseStrTable {

  /* ------------------------- Derived Parameters           ------------------------- */

  // Count the number of IVP and OVP
  def numIVP: Int = ivpsParam.length

  def numOVP: Int = ovpsParam.length

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  if (memNode.supportIndirect) {
    require(numOVP > 1, s"How you do indirect stream if you only have one output port")
  }

  /* ------------------------- Input / Output               ------------------------- */

  // I, Input, New Stream Entry from Dispatch Bus
  val newStrEntry: StreamEntry = IO(Input(new StreamEntry(memNode, ivpsParam, ovpsParam)))

  // O, Output, Memory Status
  val memStatus: MemoryNodeStatus = IO(Output(new MemoryNodeStatus))

  // I/O, Memory Read Ports, most of signal cannot be known here
  val memReadPorts: Seq[MemReadBundle] = ivpsParam.zipWithIndex.map { case (ivpParam, idx) =>
    IO(new MemReadBundle(ivpParam)).suggestName(s"MemoryReadPort$idx")
  }

  // I/O, Memory Write Ports
  val memWritePorts: Seq[MemWriteBundle] = ovpsParam.zipWithIndex.map { case (ovpParam, idx) =>
    if (memNode.needOVP) IO(Flipped(new MemWriteBundle(ovpParam))).suggestName(s"MemoryWritePort$idx")
    else WireInit(0.U.asTypeOf(new MemWriteBundle(ovpParam)))
  }

  // Output stream request, connect IVP bus
  val strResponse: StreamResponse = IO(Output(new StreamResponse(memNode, ivpsParam, ovpsParam)))

  // Output stream is accepted
  val strAccepted: Bool = IO(Input(Bool()))

  /* ------------------------- Registers                    ------------------------- */

  // Register Valid Array that indicate whether each entry is valid
  val strValids: Seq[Bool] = Seq.fill(numOVP)(RegInit(false.B))

  // Register holds Length 1D for each recurrence (write) stream
  val strLength1Ds: Seq[UInt] = Seq.fill(numOVP)(RegInit(0.U(memNode.length1DBits.W)))

  // Register holds stream data type for each recurrence (write) stream
  val strDataTypes: Seq[Option[UInt]] = Seq.fill(numOVP)({
    if (memNode.memDataTypeBits > 0) Some(RegInit(0.U(memNode.memDataTypeBits.W))) else None
  })

  // Register holds stream linear padding info
  val strPadding: Seq[Option[UInt]] = Seq.fill(numOVP) {
    if (memNode.LinearPadding) Some(RegInit(0.U(LINEAR_PADDING_BITS.W))) else None
  }

  // Register holds target input port ID for each recurrence stream
  val recIVPs: Seq[UInt] = Seq.fill(numOVP)(RegInit(0.U(log2Ceil(numIVP).W)))

  /* ------------------------- Modules                      ------------------------- */

  // Stream Response Queue for each output vector port
  val strRspQueues: Seq[WithQueueIO[StreamResponse]] =
    Seq.fill(numOVP)(Module(new QueueFPGA(new StreamResponse(memNode, ivpsParam, ovpsParam), 2, p(FPGAOverlay))))

  // Arbiter to select the one of response from stream response queue
  val recArb: RRArbiter[StreamResponse] =
    Module(new RRArbiter(new StreamResponse(memNode, ivpsParam, ovpsParam), numOVP))

  /* ------------------------- Wires                        ------------------------- */

  // At least one stream entry is valid in stream table
  val atLeastOneStrAlive: Bool = VecInit(strValids).asUInt().orR()

  // Whether this Recurrence stream is being dispatched
  val strDispatched: Seq[Bool] = for (ovpIdx <- 0 until numOVP) yield {
    newStrEntry.valid &&
    newStrEntry.memOperation.getOrElse({
      if (memNode.isREC) memOpWrite else memOpRead
    }) =/= memOpRead &&
    newStrEntry.targetLocalPortId.getOrElse(0.U) === ovpIdx.U
  }

  // OVP request ready
  val ovpReqReadies: Seq[Bool] = for (ovpIdx <- 0 until numOVP) yield {
    strValids(ovpIdx) && strRspQueues(ovpIdx).io.enq.ready
  }

  // OVP request mask for each output vector port
  val (ovpReqNums, ovpReqMasks): (Seq[UInt], Seq[UInt]) = (for (ovpIdx <- 0 until numOVP) yield {
    OVPRequestMaskGen(
      0.U((log2Ceil(memNode.bandwidth) + 1).W),
      strLength1Ds(ovpIdx),
      strDataTypes(ovpIdx),
      Some(1.U),
      memNode.bandwidth
    )
  }).unzip

  // OVP firing
  require(memWritePorts.length == ovpReqReadies.length)
  val ovpsFired: Seq[Bool] = memWritePorts.zip(ovpReqReadies).map { case (write, r) => write.ovpValid && r }

  // C, wire, calculate the memory operation that each stream entry is doing for memory status report
  //           MSB   [Atom5, Atom4,Atom3,Atom2,Atom1,Atom0,wr  , rd   ] LSB
  val strMemOp: Seq[UInt] = {
    for (ovpIdx <- 0 until numOVP) yield {
      // valid
      val valid: Bool = strValids(ovpIdx)
      // Doing Atomic Operation 5
      val aop5, aop4, aop3, aop2, aop1, aop0: Bool = false.B
      val wr, rd:                             Bool = valid
      Cat(aop5, aop4, aop3, aop2, aop1, aop0, wr, rd) & Fill(8, valid)
    }
  }

  /* ------------------------- Combination Logic            ------------------------- */

  // Connect from OVPs to each stream response queue
  for (ovpIdx <- 0 until numOVP) {
    val queue: WithQueueIO[StreamResponse] = strRspQueues(ovpIdx)
    val write: MemWriteBundle = memWritePorts(ovpIdx)
    // Valid bit
    queue.io.enq.valid := ovpsFired(ovpIdx)
    queue.io.enq.bits.valid := ovpsFired(ovpIdx)
    // Stream Response Bits
    queue.io.enq.bits.meta.targetLocalPortId match {
      case Some(value) => value := recIVPs(ovpIdx);
      case None        =>
    }
    queue.io.enq.bits.meta.memDataTypeExp match {
      case Some(value) => value := strDataTypes(ovpIdx).getOrElse(0.U);
      case None        =>
    }
    queue.io.enq.bits.meta.memOperation match {
      case Some(value) => value := memOpRead;
      case None        =>
    }
    queue.io.enq.bits.meta.linearPadding match {
      case Some(value) => value := strPadding(ovpIdx).getOrElse(0.U)
      case None        =>
    }
    queue.io.enq.bits.state match {
      case Some(value) => value := write.ovpStreamState.getOrElse(0.U.asTypeOf(new StreamState))
      case None        =>
    }
    queue.io.enq.bits.mask match {
      case Some(value) =>
        require(value.getWidth == write.ovpValidMask.getWidth)
        value := write.ovpValidMask
      case None =>
    }
    queue.io.enq.bits.readData match {
      case Some(value) =>
        require(value.getWidth == write.ovpVecData.asUInt().getWidth)
        value := write.ovpVecData.asUInt()
      case None =>
    }
  }

  // Connect the stream response queue to input of arbiter
  require(strRspQueues.length == recArb.io.in.length)
  strRspQueues.zip(recArb.io.in).foreach { case (queue, aIn) => aIn <> queue.io.deq }

  /* ------------------------- Finite State Machine         ------------------------- */

  // Stream "Table" update when OVP fired
  for (ovpIdx <- 0 until numOVP) {
    val reqNum:  UInt = (ovpReqNums(ovpIdx) >> strDataTypes(ovpIdx).getOrElse(0.U)).asUInt()
    val currL1D: UInt = strLength1Ds(ovpIdx)
    val nxtEnd:  Bool = reqNum >= currL1D
    val nxtL1D:  UInt = Mux(nxtEnd, 0.U, currL1D - reqNum)
    when(strDispatched(ovpIdx)) {
      strValids(ovpIdx) := true.B
      strLength1Ds(ovpIdx) := newStrEntry.initLength1D.getOrElse(0.U)
      strDataTypes(ovpIdx) match {
        case Some(v) => v := newStrEntry.memDataTypeExp.getOrElse(0.U)
        case None    =>
      }
      recIVPs(ovpIdx) := newStrEntry.recIVPortId.getOrElse(0.U)
      strPadding(ovpIdx) match {
        case Some(value) => value := newStrEntry.linearPadding.getOrElse(0.U)
        case None        =>
      }
    }.elsewhen(ovpsFired(ovpIdx)) {
      strValids(ovpIdx) := !nxtEnd
      strLength1Ds(ovpIdx) := nxtL1D
    }
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Connect used by memory port
  require(memWritePorts.length == strValids.length)
  require(memWritePorts.length == strDispatched.length)
  memWritePorts.zip(strValids).zip(strDispatched).foreach { case ((write, valid), dispatch) =>
    write.usedByMem := valid || dispatch
  }

  // Output to each output vector port
  for (ovpIdx <- 0 until numOVP) {
    val memReady: Bool = ovpReqReadies(ovpIdx)
    // Ready bit
    memWritePorts(ovpIdx).memReady := memReady
    // Ready mask
    memWritePorts(ovpIdx).memReadyMask := Mux(memReady, ovpReqMasks(ovpIdx), 0.U)
    // New memory data type and new length 1D
    memWritePorts(ovpIdx).newMemDataType := strDispatched(ovpIdx)
    memWritePorts(ovpIdx).memLength1D.valid := strDispatched(ovpIdx)
    memWritePorts(ovpIdx).memDataType match {
      case Some(value) => value := newStrEntry.memDataTypeExp.getOrElse(0.U)
      case None        =>
    }
    memWritePorts(ovpIdx).memLength1D.bits := newStrEntry.initLength1D.getOrElse(0.U)
  }

  // Connect the arbiter output to stream response
  strResponse := recArb.io.out.bits
  strResponse.valid := recArb.io.out.valid
  recArb.io.out.ready := strAccepted

  // Input Vector Port Ignore, since response to input vector port is handle by stream read bus
  memReadPorts.zipWithIndex.foreach { case (ivpPort, ivpIdx) =>
    ivpPort.memValid := DontCare
    ivpPort.memValidMask := DontCare
    ivpPort.memData := DontCare
    // If Stream Table belongs to recurrence engine, we should check OVP part of stream table
    ivpPort.usedByMem := VecInit(recIVPs.zip(strValids).map { case (recIVP, valid) =>
      valid && recIVP === ivpIdx.U
    }).asUInt().orR
    ivpPort.memStreamState match {
      case Some(value) => value := DontCare
      case None        =>
    }
    ivpPort.memPadding match {
      case Some(value) => value := DontCare
      case None        =>
    }
    if (ivpPort.broadcastReset.isDefined) ivpPort.broadcastReset.get := DontCare
  }

  // Report memory status to stream dispatcher
  memStatus.alive := atLeastOneStrAlive
  memStatus.newStr := newStrEntry.valid
  memStatus.aguReq := strResponse.valid && atLeastOneStrAlive
  memStatus.readPause := recArb.io.out.valid && !strAccepted && atLeastOneStrAlive
  memStatus.writePause := VecInit(strRspQueues.map(!_.io.enq.ready)).asUInt().orR() && atLeastOneStrAlive
  memStatus.portPause := VecInit(memWritePorts.map(_.ovpAvailUnits === 0.U)).asUInt().andR() &&
    VecInit(memReadPorts.map(_.ivpAvailUnits === 0.U)).asUInt().andR() && atLeastOneStrAlive
  memStatus.memType := recMemType
  memStatus.doingAtomOp5 := VecInit(strMemOp.map(_.apply(7))).asUInt().orR()
  memStatus.doingAtomOp4 := VecInit(strMemOp.map(_.apply(6))).asUInt().orR()
  memStatus.doingAtomOp3 := VecInit(strMemOp.map(_.apply(5))).asUInt().orR()
  memStatus.doingAtomOp2 := VecInit(strMemOp.map(_.apply(4))).asUInt().orR()
  memStatus.doingAtomOp1 := VecInit(strMemOp.map(_.apply(3))).asUInt().orR()
  memStatus.doingAtomOp0 := VecInit(strMemOp.map(_.apply(2))).asUInt().orR()
  memStatus.doingWrite := VecInit(strMemOp.map(_.apply(1))).asUInt().orR()
  memStatus.doingRead := VecInit(strMemOp.map(_.apply(0))).asUInt().orR()

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  /* ------------------------- Utility                      ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */

  // The vector length of output vector port should be the same, equal to bandwidth
  memWritePorts.zipWithIndex.foreach { case (writePort, i) =>
    require(
      writePort.ovpVecData.length == memNode.bandwidth,
      s"The $i write port's width is not equal to memory bandwidth"
    )
  }
}
