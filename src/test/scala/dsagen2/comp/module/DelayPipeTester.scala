package dsagen2.comp.module

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.util.VcsSimulationHelper
import org.scalatest.flatspec.AnyFlatSpec

class DelayPipeTester extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {

  // Set up parameters
  val maxDelay : Int = 16
  val bits : Int = 8
  val test_length : Int = 32
  val r = new scala.util.Random

  // Generate Delay and Input
  val delay_list: Seq[Int] = Seq.fill(test_length)(r.nextInt(maxDelay))
  val bits_list: Seq[BigInt] = Seq.fill(test_length)(BigInt(bits-1, r))

  "delay pipe" should "delay input by certain cycles" in {
    test(new DelayPipe[UInt](gen = UInt(bits.W), maxDelay = maxDelay))
      .withAnnotations(VcsDirAnnotations){ dut =>
        dut.input.initSource().setSourceClock(dut.clock)
        dut.output.initSink().setSinkClock(dut.clock)
        // Loop over all delay
        for(delay <- delay_list){
          // Poke delay
          dut.delay.poke(delay.U)
          // Poke all inputs
          for(bits <- bits_list){
            dut.input.enqueueNow(bits.U)
            dut.clock.step(r.nextInt(5))
          }
          // Change to next delay
          dut.clock.step(2)
        }
      }
  }
}
