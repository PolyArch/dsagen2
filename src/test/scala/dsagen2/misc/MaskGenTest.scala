package dsagen2.misc

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util.log2Ceil
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.misc.module.SmallMaskGen
import dsagen2.util.VcsSimulationHelper
import logger.{LogLevel, LogLevelAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

class MaskGenTest extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {
  "Mask Gen of Rocketchip" should "work as I expect" in {
    val memWidthSeq : Seq[Int] = Seq(2, 4, 8, 16)

    for(memWidth <- memWidthSeq){
      test(new SmallMaskGen(memWidth)).withAnnotations(VcsDirAnnotations){dut =>
        for(addr <- 0 until memWidth; lgSize <- 0 until log2Ceil(memWidth) + 2){
          dut.addr.poke(addr.U)
          dut.lgSize.poke(lgSize.U)
          val mask : BigInt = dut.mask.peek().litValue()
          //println(s"memWidth = $memWidth, addr = $addr, lgSize = $lgSize, mask = ${mask.toString(2)}")
        }
      }
    }

    // Generate Verilog
    (new ChiselStage).emitVerilog(
      new SmallMaskGen(8),
      Array("--full-stacktrace", "--target-dir", "vsrc"),
      Seq(LogLevelAnnotation(LogLevel.Error))
    )
  }
}
