package dsagen2.misc

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.misc.module.MaskExpExtender
import dsagen2.util.VcsSimulationHelper
import org.scalatest.flatspec.AnyFlatSpec

class ExtendMaskByExpTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {
  "Mask extender" should "work as expected" in {
    test(new MaskExpExtender(8, 4)).withAnnotations(VcsDirAnnotations){ dut =>
      // Test
      dut.clock.step(1)
      dut.inputMask.poke("b00001101".U)
      dut.exp.get.poke(0.U)
      dut.outputMask.expect("b00001101".U)
      // Test
      dut.clock.step(1)
      dut.inputMask.poke("b00000101".U)
      dut.exp.get.poke(1.U)
      dut.outputMask.expect("b00001111".U)
      // Test
      dut.clock.step(1)
      dut.inputMask.poke("b00010001".U)
      dut.exp.get.poke(1.U)
      dut.outputMask.expect("b00110011".U)
      // Test
      dut.clock.step(1)
      dut.inputMask.poke("b00000001".U)
      dut.exp.get.poke(2.U)
      dut.outputMask.expect("b00001111".U)
      // Test
      dut.clock.step(1)
      dut.inputMask.poke("b00010000".U)
      dut.exp.get.poke(2.U)
      dut.outputMask.expect("b11110000".U)
      // Test
      dut.clock.step(1)
      dut.inputMask.poke("b00000001".U)
      dut.exp.get.poke(3.U)
      dut.outputMask.expect("b11111111".U)
      // Test
      dut.clock.step(1)
      dut.inputMask.poke("b00010000".U)
      dut.exp.get.poke(3.U)
      dut.outputMask.expect("b00000000".U)
    }
  }
}
