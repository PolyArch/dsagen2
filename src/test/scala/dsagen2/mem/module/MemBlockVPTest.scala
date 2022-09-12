package dsagen2.mem.module

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.ChiselScalatestTester
import dsagen2.mem.config.DefaultDMAConfig
import dsagen2.util.VcsSimulationHelper
import dsagen2.sync.module.MemBlockMultiIOQueue
import org.scalatest.flatspec.AnyFlatSpec

class MemBlockVPTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {
  "SyncReadMem based vector port" should "work as expected" in {
    test(new MemBlockMultiIOQueue[UInt](UInt(8.W),
      numInput = 4, numOutput = 8,
      inputSync = true, outputSync = true, depth = 16)(new DefaultDMAConfig)).withAnnotations(VcsDirAnnotations){dut =>
      // cycle 1
      dut.vecInput.zipWithIndex.foreach{
        case (input, idx) =>
          input.bits.poke(idx.U)
          input.valid.poke(true.B)
      }
      dut.vecInputSync match {
        case Some(seqSync) => seqSync.foreach(s => s.poke(true.B))
        case None =>
      }
      dut.vecOutput.zipWithIndex.foreach{ case (output, idx) =>
        if(idx < dut.numInput) output.ready.poke(true.B)
      }
      dut.vecOutputSync match {
        case Some(seqSync) => seqSync.foreach(s => s.poke(false.B))
        case None =>
      }
      // cycle 3
      dut.clock.step(2)
      dut.vecOutput.zipWithIndex.foreach{
        case (output, idx) =>
          if(idx < dut.numInput) {
            output.bits.expect(idx.U)
            output.valid.expect(true.B)
          } else {
            output.valid.expect(false.B)
          }
      }
    }
  }

}
