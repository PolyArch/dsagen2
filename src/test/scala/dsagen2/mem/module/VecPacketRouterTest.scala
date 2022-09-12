package dsagen2.mem.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.mem.module.xbar.VecPacketRouter
import dsagen2.util.VcsSimulationHelper
import org.scalatest.flatspec.AnyFlatSpec

class VecPacketRouterTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {
  implicit val p : Parameters = Parameters.empty

  "Vector of packet router" should "solves destination conflict" in {
    test(new VecPacketRouter(UInt(3.W), 4, depth = 2)).withAnnotations(VcsDirAnnotations) { dut =>

      /* ------ Cycle 0 ------ */
      // Poke input bits
      dut.vecInput.zipWithIndex.foreach { case (vecIn, idx) =>
        vecIn.valid.poke(true.B)
        vecIn.bits.poke(idx.U)
      }
      // Poke output ready
      dut.vecOutput.foreach(o => o.ready.poke(true.B))
      // Poke routing
      dut.vecRoute(0).poke(2.U)
      dut.vecRoute(1).poke(1.U)
      dut.vecRoute(2).poke(2.U) // one conflict
      dut.vecRoute(3).poke(0.U)
      // Expect all ready is false since there is one conflict
      dut.vecInput.foreach(i => i.ready.expect(false.B))

      /* ------ Cycle 2 ------ */
      // Step
      dut.clock.step(1)
      // Expect first set of conflict result
      dut.vecOutput(0).bits.expect(3.U)
      dut.vecOutput(1).bits.expect(1.U)
      dut.vecOutput(2).bits.expect(2.U)
      dut.vecOutput(0).valid.expect(true.B)
      dut.vecOutput(1).valid.expect(true.B)
      dut.vecOutput(2).valid.expect(true.B)
      dut.vecOutput(3).valid.expect(false.B)
      // Expect all ready is true since conflict is solved
      dut.vecInput.foreach(i => i.ready.expect(true.B))

      /* ------ Cycle 3 ------ */
      // Step
      dut.clock.step(1)
      // Expect the last packet
      dut.vecOutput(2).bits.expect(0.U)
      dut.vecOutput(0).valid.expect(false.B)
      dut.vecOutput(1).valid.expect(false.B)
      dut.vecOutput(2).valid.expect(true.B)
      dut.vecOutput(3).valid.expect(false.B)
    }
  }
}
