package dsagen2.misc

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.misc.module.MaskDistributor
import dsagen2.util.VcsSimulationHelper
import org.scalatest.flatspec.AnyFlatSpec

class MaskDisTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper{
  "Mask Distributor" should "behaves as expected" in {
    test(new MaskDistributor(UInt(4.W), 4)).withAnnotations(VcsDirAnnotations){dut =>
      dut.io.sources(0).bits.poke(1.U)
      dut.io.sources(1).bits.poke(5.U)
      dut.io.sources(2).bits.poke(2.U)
      dut.io.sources(3).bits.poke(3.U)
      dut.io.sources.head.valid.poke(false.B)
      dut.io.sources(1).valid.poke(true.B)
      dut.io.sources(2).valid.poke(false.B)
      dut.io.sources(3).valid.poke(true.B)
      dut.io.sinks(0).bits.expect(0.U)
      dut.io.sinks(1).bits.expect(1.U)
      dut.io.sinks(2).bits.expect(0.U)
      dut.io.sinks(3).bits.expect(5.U)
      dut.io.sinks(0).valid.expect(false.B)
      dut.io.sinks(1).valid.expect(true.B)
      dut.io.sinks(2).valid.expect(false.B)
      dut.io.sinks(3).valid.expect(true.B)
    }
  }
}
