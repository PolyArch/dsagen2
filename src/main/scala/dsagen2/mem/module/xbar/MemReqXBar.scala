package dsagen2.mem.module.xbar

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.MemRequest
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.module.MultiIOQueue
import dsagen2.util.{QueueFPGA, WithQueueIO}

/** Indirect enabled Banked Scratchpad needs a crossbar to solve bank conflict when request to banked scratchpad
  *
  * This is XBar solves bank conflict by using an multi-enqueue single-dequeue queue (vector port)
  *
  * @param memNode Memory Node Parameters
  */
class MemReqXBar(memNode: MemNodeParameters)(implicit val p: Parameters) extends MultiIOModule {

  /* -------------------------      Extract Parameters      ------------------------- */

  // Number of Request Port
  val numPort: Int = memNode.numMemReqPort

  // Scratchpad Bank Width
  val bankWidth: Int = memNode.spmBankWidth

  // Scratchpad Memory Width
  val memWidth: Int = memNode.bandwidth

  // Scratchpad Number of Bank
  val numBank: Int = memNode.numSpmBank

  // Implementation, TODO: hard-coded, we should make it a exploration option in future
  val vpImpl :: arbImpl :: Nil = (0 until 2).toList
  val impl: Int = arbImpl
  /* -------------------------     Derived Parameters       ------------------------- */

  // Number of bits for row offset within bank row
  val rowOffsetBits: Int = log2Ceil(bankWidth)

  // Number of bits for bank selection
  val bankSelBits: Int = log2Ceil(numBank)

  /* -------------------------      Parameters Sanity Check ------------------------- */

  // Only Banked Scratchpad with Indirect Index Stream Pattern enabled will create this module
  require(memNode.isSPM && memNode.IndirectIndexStream, s"This memory node $memNode seems not need Req Xbar")

  // Memory Width = #bank x bankWidth
  require(memWidth == bankWidth * numBank, s"$memWidth != $bankWidth x $numBank")

  /* -------------------------         Input / Output       ------------------------- */

  // Vector of Memory Request
  val vecInput:      Vec[MemRequest] = IO(Input(Vec(numPort, new MemRequest(memNode))))
  val vecInputReady: Vec[Bool] = IO(Output(Vec(numPort, Bool())))

  // Vector of Memory Request with back conflict solved
  val vecOutput:      Vec[MemRequest] = IO(Output(Vec(numPort, new MemRequest(memNode))))
  val vecOutputReady: Vec[Bool] = IO(Input(Vec(numPort, Bool())))

  // O, Pause
  val pause: Bool = IO(Output(Bool()))

  /* -------------------------         Implementations       ------------------------- */

  // A vector port based implementation
  if (impl == vpImpl) {
    /* -------------------------     Registers and Modules    ------------------------- */

    // C, Multi-IO Queue to solve bank conflict
    // TODO: valid bit can be optimized out
    val bankQueues: Seq[MultiIOQueue] = Seq.fill(numPort)(
      Module(
        new MultiIOQueue(
          numInput = numPort,
          inputBits = vecInput.head.getWidth,
          numOutput = 1,
          outputBits = vecOutput.head.getWidth,
          depthInMinBits = 2 * numPort // TODO: make it reconfigurable from memory node parameter
        )
      )
    )
    // We don't support memory reqeust repeating for now, TODO: to be discussed?
    bankQueues.foreach(q => q.repeatDequeue := false.B)

    // C, Wires that hold whether the this request port is ready
    val reqReadies: Vec[Bool] = WireInit(VecInit(Seq.fill(numPort)(false.B)))

    /* -------------------------             Wires            ------------------------- */

    // C, Wires that holds the bank selection of each request port
    require(vecInput.head.vaddr.getWidth >= bankSelBits + rowOffsetBits)
    val bankSel: Seq[UInt] = vecInput.map(_.vaddr.apply(bankSelBits + rowOffsetBits - 1, rowOffsetBits))

    /* -------------------------     Combinational Logics     ------------------------- */

    // Connect Request Ready of each port
    reqReadies.zipWithIndex.foreach { case (ready, idx) =>
      // Get the idx request port of each queue
      val queueReady: Seq[Bool] = bankQueues.map(_.vecInput(idx).ready)
      // queue of all bank queue should be all ready, since request can go to any bank
      ready := VecInit(queueReady).asUInt().andR()
    }

    // Connect the bits (request) and valid (request valid and bank selection) to each input of bank queue
    vecInput.zipWithIndex.foreach { case (request, idx) =>
      bankQueues.zipWithIndex.foreach { case (queue, bankIdx) =>
        require(queue.vecInput(idx).bits.getWidth == request.getWidth)
        queue.vecInput(idx).bits := request.asUInt()
        /* This is where bank selection happens, if bankSel match the bank Idx, it will be valid*/
        queue.vecInput(idx).valid := request.valid && bankSel(idx) === bankIdx.U
      }
    }

    // Always dequeue from each queue, since each bank is fully pipelined and backpressure is solved when selection
    bankQueues.foreach { queue =>
      require(queue.vecOutput.length == 1, s"I believe each bank only has one request port")
      queue.vecOutput.head.ready := true.B
    }

    /* -------------------------     Finite State Machine     ------------------------- */

    /* -------------------------       Output Connection      ------------------------- */

    // AGU should be pause if any request is not ready (or we can say pause if not all ready)
    pause := !reqReadies.asUInt().andR()

    // Connect to Vector Memory Request Port
    require(vecOutput.length == bankQueues.length)
    vecOutput.zip(bankQueues).foreach { case (request, queue) =>
      // The bits is all we need already, valid should be identical with the valid in bits
      request := queue.vecOutput.head.bits.asTypeOf(new MemRequest(memNode))
    }
  } else if (impl == arbImpl) { // A arbiter based implementation
    // Create Packet Routing Module, queue depth = half pending request (because there is an Response XBar use the other half)
    val depth: Int = (numBank + 2).max(memNode.numPendingRequest / 2)
    val reqQueue: Seq[WithQueueIO[MemRequest]] =
      Seq.fill(numBank)(Module(new QueueFPGA[MemRequest](new MemRequest(memNode), depth, p(FPGAOverlay))))
    val reqRouter: VecPacketRouter[MemRequest] =
      Module(new VecPacketRouter[MemRequest](new MemRequest(memNode), numBank, 2))
    // Connect the input side
    reqQueue.zip(vecInput).zip(vecInputReady).foreach { case ((queue, in), inReady) =>
      queue.io.enq.bits := in
      queue.io.enq.valid := in.valid
      inReady := queue.io.enq.ready
    }
    // Connect each queue to each input of router
    reqQueue.zip(reqRouter.vecInput).foreach { case (qOut, rIn) => rIn <> qOut.io.deq }

    // Extract the bank selection from output of queue
    require(reqQueue.head.io.deq.bits.vaddr.getWidth >= bankSelBits + rowOffsetBits)
    val bankSel: Seq[UInt] = reqQueue.map(_.io.deq.bits.vaddr.apply(bankSelBits + rowOffsetBits - 1, rowOffsetBits))

    // Connect the routing
    require(reqRouter.vecRoute.length == bankSel.length)
    reqRouter.vecRoute.zip(bankSel).foreach { case (route, sel) =>
      require(route.getWidth == sel.getWidth)
      route := sel
    }

    // Connect the output side
    reqRouter.vecOutput.zip(vecOutput).zip(vecOutputReady).foreach { case ((routeOut, out), outReady) =>
      out := routeOut.bits
      out.valid := routeOut.valid
      routeOut.ready := outReady
    }

    // Connect the pause, stop requesting is there is only 1 left
    pause := VecInit(reqQueue.map(q => q.io.count >= (depth - 1).U)).asUInt().orR()
  }
}
