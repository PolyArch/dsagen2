package dsagen2.misc

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.misc.module.Mask2Routing
import dsagen2.util.VcsSimulationHelper
import org.scalatest.flatspec.AnyFlatSpec

class Mask2RoutingTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {

  "Input Mask to Output Routing" should "works" in {
    // Test mask as input mask
    test(new Mask2Routing(4, asInput = true)).withAnnotations(VcsDirAnnotations){ dut =>
        dut.mask.poke("b1101".U)
        dut.outRouting.head.expect(1.U)
        dut.outRouting(1).expect(3.U)
        dut.outRouting(2).expect(4.U)
        dut.outRouting(3).expect(0.U)
        dut.clock.step(1)
        dut.mask.poke("b0000".U)
        dut.outRouting.head.expect(0.U)
        dut.outRouting(1).expect(0.U)
        dut.outRouting(2).expect(0.U)
        dut.outRouting(3).expect(0.U)
        dut.clock.step(1)
        dut.mask.poke("b0010".U)
        dut.outRouting.head.expect(2.U)
        dut.outRouting(1).expect(0.U)
        dut.outRouting(2).expect(0.U)
        dut.outRouting(3).expect(0.U)
        dut.clock.step(1)
        dut.mask.poke("b1111".U)
        dut.outRouting.head.expect(1.U)
        dut.outRouting(1).expect(2.U)
        dut.outRouting(2).expect(3.U)
        dut.outRouting(3).expect(4.U)
        dut.clock.step(1)
      }
  }

  "Output Mask to Output Routing" should "works" in {
    // Test mask as output mask
    test(new Mask2Routing(4, asInput = false)).withAnnotations(VcsDirAnnotations){ dut =>
        dut.mask.poke("b1011".U)
        dut.outRouting.head.expect(1.U)
        dut.outRouting(1).expect(2.U)
        dut.outRouting(2).expect(0.U)
        dut.outRouting(3).expect(3.U)
        dut.clock.step(1)
        dut.mask.poke("b0000".U)
        dut.outRouting.head.expect(0.U)
        dut.outRouting(1).expect(0.U)
        dut.outRouting(2).expect(0.U)
        dut.outRouting(3).expect(0.U)
        dut.clock.step(1)
        dut.mask.poke("b1010".U)
        dut.outRouting.head.expect(0.U)
        dut.outRouting(1).expect(1.U)
        dut.outRouting(2).expect(0.U)
        dut.outRouting(3).expect(2.U)
        dut.clock.step(1)
        dut.mask.poke("b1111".U)
        dut.outRouting.head.expect(1.U)
        dut.outRouting(1).expect(2.U)
        dut.outRouting(2).expect(3.U)
        dut.outRouting(3).expect(4.U)
        dut.clock.step(1)
      }
  }

}
