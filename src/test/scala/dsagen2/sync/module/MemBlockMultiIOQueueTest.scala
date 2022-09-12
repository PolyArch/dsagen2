package dsagen2.sync.module

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.mem.config.DefaultDMAConfig
import dsagen2.util.VcsSimulationHelper
import org.scalatest.flatspec.AnyFlatSpec

class MemBlockMultiIOQueueTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {
  "MemBlockMultiIOQueue" should "function as a circular multi IO queue" in {
    test(new MemBlockMultiIOQueue[UInt](UInt(8.W), 4, 4, false, false, 16)(new DefaultDMAConfig)).withAnnotations(VcsDirAnnotations) {
      dut =>
        dut.vecInput(0).valid.poke(true.B)
        dut.vecInput(0).bits.poke(1.U)
        dut.clock.step(1)
        dut.vecInput(0).valid.poke(true.B)
        dut.vecInput(1).valid.poke(true.B)
        dut.vecInput(0).bits.poke(2.U)
        dut.vecInput(1).bits.poke(2.U)
        dut.clock.step(1)
        dut.vecInput(0).valid.poke(true.B)
        dut.vecInput(1).valid.poke(true.B)
        dut.vecInput(2).valid.poke(true.B)
        dut.vecInput(0).bits.poke(3.U)
        dut.vecInput(1).bits.poke(3.U)
        dut.vecInput(2).bits.poke(3.U)
        dut.clock.step(2)
        dut.vecOutput(0).valid.expect(false.B)
    }
  }
}
