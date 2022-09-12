package dsagen2.mem.module.xbar

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.MemResponse
import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.module.MultiIOQueue
import dsagen2.util.{QueueFPGA, WithQueueIO}

/** Indirect Banked Scratchpad needs a crossbar to route memory response back to right vector position
  *
  * @param memNode Memory Node Parameters
  */
class MemRspXBar(memNode: MemNodeParameters)(implicit val p: Parameters) extends MultiIOModule {

  /* -------------------------      Extract Parameters      ------------------------- */

  // Number of Request Port
  val numPort: Int = memNode.numMemReqPort

  // Implementation, TODO: for now it is hard-coded, since vp implementation cannot pass synthesis
  val vpImpl :: arbImpl :: Nil = (0 until 2).toList
  val impl: Int = arbImpl

  /* -------------------------     Derived Parameters       ------------------------- */

  /* -------------------------      Parameters Sanity Check ------------------------- */

  // Only Banked Scratchpad with Indirect Index Stream Pattern enabled will create this module
  require(memNode.isSPM && memNode.IndirectIndexStream, s"This memory node $memNode seems not need Req Xbar")

  /* -------------------------         Input / Output       ------------------------- */

  // Vector of Memory Request
  val vecInput:      Vec[MemResponse] = IO(Input(Vec(numPort, new MemResponse(memNode))))
  val vecInputReady: Vec[Bool] = IO(Output(Vec(numPort, Bool())))

  // Vector of Memory Request with back conflict solved
  val vecOutput:      Vec[MemResponse] = IO(Output(Vec(numPort, new MemResponse(memNode))))
  val vecOutputReady: Vec[Bool] = IO(Input(Vec(numPort, Bool())))

  // O, Pause
  val pause: Bool = IO(Output(Bool()))

  // Implementation
  if (impl == vpImpl) {

    /* -------------------------             Wires            ------------------------- */

    // C, Wires that holds the bank selection of each request port
    require(vecInput.head.reqIdx.isDefined)
    require(vecInput.head.reqIdx.get.getWidth == log2Ceil(numPort))
    val reqVecSel: Seq[UInt] = vecInput.map(_.reqIdx.get)

    /* -------------------------     Registers and Modules    ------------------------- */

    // C, Multi-IO Queue to solve bank conflict
    // TODO: valid bit can be optimized out
    val reqVecQueues: Seq[MultiIOQueue] = Seq.fill(numPort)(
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
    // Memory response repeating is not supported for now, TODO: to be discussed, do we want it?
    reqVecQueues.foreach(q => q.repeatDequeue := false.B)

    // C, Wires that hold whether the this request port is ready
    val reqReadies: Vec[Bool] = WireInit(VecInit(Seq.fill(numPort)(false.B)))

    /* -------------------------     Combinational Logics     ------------------------- */

    // Connect Request Ready of each port
    reqReadies.zipWithIndex.foreach { case (ready, idx) =>
      // Get the idx request port of each queue
      val queueReady: Seq[Bool] = reqVecQueues.map(_.vecInput(idx).ready)
      // queue of all queue should be all ready, since request can go to any position of request vector
      ready := VecInit(queueReady).asUInt().andR()
    }

    // Connect the bits (response) and valid (request valid and request vector selection) to each input of queue
    vecInput.zipWithIndex.foreach { case (response, idx) =>
      reqVecQueues.zipWithIndex.foreach { case (queue, reqIdx) =>
        require(queue.vecInput(idx).bits.getWidth == response.getWidth)
        queue.vecInput(idx).bits := response.asUInt()
        /* This is where bank selection happens, if bankSel match the bank Idx, it will be valid*/
        queue.vecInput(idx).valid := response.valid && reqVecSel(idx) === reqIdx.U
      }
    }

    // Always dequeue from each queue, backpressure is solved when selection
    reqVecQueues.foreach { queue =>
      require(queue.vecOutput.length == 1, s"I believe each bank only has one request port")
      queue.vecOutput.head.ready := true.B
    }

    /* -------------------------     Finite State Machine     ------------------------- */

    /* -------------------------       Output Connection      ------------------------- */

    // AGU should be pause if any request is not ready (or we can say pause if not all ready)
    pause := !reqReadies.asUInt().andR()

    // Connect to Vector Memory Request Port
    require(vecOutput.length == reqVecQueues.length)
    vecOutput.zip(reqVecQueues).foreach { case (request, queue) =>
      // The bits is all we need already, valid should be identical with the valid in bits
      request := queue.vecOutput.head.bits.asTypeOf(new MemResponse(memNode))
    }
  } else if (impl == arbImpl) { // A arbiter based
    // Create packet routing module
    val depth: Int = (numPort + 2).max(memNode.numPendingRequest / 2)
    val rspQueues: Seq[WithQueueIO[MemResponse]] =
      Seq.fill(numPort)(Module(new QueueFPGA[MemResponse](new MemResponse(memNode), depth, p(FPGAOverlay))))
    val rspRouter: VecPacketRouter[MemResponse] =
      Module(new VecPacketRouter[MemResponse](new MemResponse(memNode), numPort, 2))

    // Connect input side to each queue
    rspQueues.zip(vecInput.zip(vecInputReady)).foreach { case (queue, (in, inReady)) =>
      queue.io.enq.bits := in
      queue.io.enq.valid := in.valid
      inReady := queue.io.enq.ready
    }

    // Connect each queue to each input of router
    rspQueues.zip(rspRouter.vecInput).foreach { case (qOut, rIn) => rIn <> qOut.io.deq }

    // Extract the port selection from the outout of queue
    require(rspQueues.head.io.deq.bits.reqIdx.isDefined)
    require(rspQueues.head.io.deq.bits.reqIdx.get.getWidth == log2Ceil(numPort))
    val vecSel: Seq[UInt] = rspQueues.map(_.io.deq.bits.reqIdx.get)

    // Connect the routing
    require(vecSel.length == rspRouter.vecRoute.length)
    rspRouter.vecRoute.zip(vecSel).foreach { case (route, sel) =>
      require(route.getWidth == sel.getWidth)
      route := sel
    }

    // Connect each output of router to output side
    rspRouter.vecOutput.zip(vecOutput.zip(vecOutputReady)).foreach { case (rOut, (out, outReady)) =>
      out := rOut.bits
      out.valid := rOut.valid
      rOut.ready := outReady
    }

    // Connect the pause, stop requesting is there is only 1 left
    pause := VecInit(rspQueues.map(q => q.io.count >= (depth - 1).U)).asUInt().orR()
  }

}
