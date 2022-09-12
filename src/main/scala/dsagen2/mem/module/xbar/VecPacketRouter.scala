package dsagen2.mem.module.xbar

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.internal.requireIsChiselType
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.util.{QueueFPGA, WithQueueIO}

/** This implementation solves packet destination conflict by using N Arbiter (lower first) and N Standard Queue
  */
class VecPacketRouter[T <: Data](val gen: T, numPck: Int, depth: Int)(implicit val p: Parameters)
    extends MultiIOModule {
  /* ------------------------- Derived Parameters           ------------------------- */

  // Get the hardware type
  val genType: T = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  // Number of bits needed for destination routing
  def routeIdxBits: Int = if (numPck > 1) log2Ceil(numPck) else 0

  /* ------------------------- Input / Output               ------------------------- */

  // Vector Input
  val vecInput: Vec[DecoupledIO[T]] = IO(Flipped(Vec(numPck, DecoupledIO(genType))))

  // Vector Output
  val vecOutput: Vec[DecoupledIO[T]] = IO(Vec(numPck, DecoupledIO(genType)))

  // Vector Routing
  val vecRoute: Vec[UInt] =
    if (routeIdxBits > 0) IO(Input(Vec(numPck, UInt(routeIdxBits.W)))) else IO(Input(Vec(numPck, UInt(1.W))))

  // Vector Count, count for each queue
  val vecCount: Vec[UInt] = IO(Output(Vec(numPck, UInt(log2Ceil(depth + 1).W))))

  /* ------------------------- Registers                    ------------------------- */
  // Register that indicate whether each position is routed
  val vecRouted: Vec[Bool] = RegInit(VecInit(Seq.fill(numPck)(false.B)))

  /* ------------------------- Modules                      ------------------------- */
  // Arbiters that solves the conflict
  val vecArbiter: Seq[RRArbiter[T]] = Seq.fill(numPck)(Module(new RRArbiter[T](genType, numPck)))

  // Queues that buffer the packet
  val vecQueue: Seq[WithQueueIO[T]] = Seq.fill(numPck)(Module(new QueueFPGA[T](genType, depth, p(FPGAOverlay))))

  /* ------------------------- Wires                        ------------------------- */

  // Collect the ready OR reduce signal for each position
  val vecAccepted: Seq[Bool] = (0 until numPck).map { vecIdx =>
    VecInit(vecArbiter.map(a => a.io.in(vecIdx).fire())).asUInt().orR()
  }

  // Check whether all packets are routed
  val vecFinished: Seq[Bool] = vecInput.zip(vecAccepted).zip(vecRouted).map { case ((vecIn, accepted), routed) =>
    // Finished means : either there is no request, or accepted this cycle, or already routed in previous cycles
    !vecIn.valid || accepted || routed
  }

  // All packet are finished routing
  val allFinished: Bool = VecInit(vecFinished).asUInt().andR()

  // Valid packets request exist
  val existPacket: Bool = VecInit(vecInput.map(_.valid)).asUInt().orR()

  /* ------------------------- Combination Logic            ------------------------- */

  // Turn off vector input ready by false, by default
  vecInput.foreach(i => i.ready := allFinished)

  // Connect packet routing to each arbiter
  vecArbiter.zipWithIndex.foreach { case (arb, destIdx) =>
    arb.io.in.zip(vecInput).zip(vecRoute).zip(vecRouted).foreach { case (((arbIn, vecIn), route), routed) =>
      // Connect bits
      arbIn.bits := vecIn.bits
      // Connect valid, valid input exist, routing is correct, not routed yet
      arbIn.valid := vecIn.valid && destIdx.U === route && !routed
    }
  }

  // Connect arbiter output to each queue
  vecArbiter.zip(vecQueue).zip(vecRouted).foreach { case ((arb, q), routed) => q.io.enq <> arb.io.out }

  // Connect queue output to each output
  vecQueue.zip(vecOutput).foreach { case (q, out) => q.io.deq <> out }

  /* ------------------------- Finite State Machine         ------------------------- */

  // FSM: switch the routed state for each position
  vecRouted.zip(vecAccepted).foreach { case (routed, acc) =>
    // Not routed -> routed
    when(!routed) { // This position has not been routed
      // Switch state when current vector of packet cannot be routed within this cycle
      // and this position is accepted
      when(!allFinished && acc) {
        routed := true.B
      }
    }.otherwise { // This position has been routed
      when(allFinished) {
        routed := false.B
      }
    }
  }

  /* -------------------------       Output Connection      ------------------------- */

  // Output the count
  vecCount.zip(vecQueue).foreach { case (count, queue) =>
    require(count.getWidth == queue.io.count.getWidth)
    count := queue.io.count
  }
}
