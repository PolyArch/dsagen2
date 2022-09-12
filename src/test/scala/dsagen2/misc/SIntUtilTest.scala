package dsagen2.misc

import chisel3._
import chisel3.stage.ChiselStage
import chiseltest._
import dsagen2.misc.module.SIntExtender
import dsagen2.util.VcsSimulationHelper
import firrtl.options.TargetDirAnnotation
import logger.{LogLevel, LogLevelAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

class SIntUtilTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper{
  "Chisel" should "do sign extending for us" in {
    test(new SIntExtender(4, 7)) {dut =>
      dut.input.poke("b1110".U)
      dut.output.expect("b1111110".U)
    }

    (new ChiselStage).emitVerilog(
      new SIntExtender(4, 7),
      Array("--full-stacktrace", "--target-dir", "vsrc"),
      Seq(LogLevelAnnotation(LogLevel.Error))
    )
  }
}
