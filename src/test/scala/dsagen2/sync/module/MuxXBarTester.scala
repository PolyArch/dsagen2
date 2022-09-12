package dsagen2.sync.module

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import dsagen2.util.VcsSimulationHelper
import org.scalatest.flatspec.AnyFlatSpec

class MuxXBarTester extends AnyFlatSpec with ChiselScalatestTester with VcsSimulationHelper {
  val maxValue = 64
  val numInput : Int = 32
  val numOutput : Int = 16

  val r = new scala.util.Random
  val randTestLength : Int = 64 //number of random input tests

  var input_list : Seq[Int] = Seq.fill(numInput)(r.nextInt(maxValue))
  var sel_list : Seq[Int] = Seq.fill(numOutput)(r.nextInt(numInput+1))

  "MuxXBarTester" should "act as a multiplexer for each output" in {
    test(new MuxXBar[UInt](gen = UInt(log2Ceil(maxValue).W), numInput = numInput,
      numOutput = numOutput)).withAnnotations(VcsDirAnnotations) { dut =>
      // unit test with unused ports //
      val randInput : Int = r.nextInt(numInput)
      val randOutput : Int = r.nextInt(numOutput)
      dut.vecInput(randInput).bits.poke(input_list.head.U)
      dut.vecInput(randInput).valid.poke(true.B)
      dut.sels(randOutput).poke((randInput+1).U)
      dut.vecOutput(randOutput).ready.poke(true.B)
      dut.vecOutput(randOutput).bits.expect(input_list.head.U)

      // test all inputs for all outputs //
      dut.vecInput.zip(input_list).foreach{ case (input, value) =>
        input.bits.poke(value.U)
        input.valid.poke(true.B)
      }
      //set all outputs readies to true
      dut.vecOutput.foreach{ port => port.ready.poke(true.B) }

      //test all values of sels for each output sequentially
      dut.vecOutput.zip(dut.sels).foreach { case (output, sels) =>
        for (idx <- 0 to numInput) {
          sels.poke(idx.U)
          if (idx == 0) { //nowhere tests
            output.bits.expect(0.U)
          } else { //all inputs
            output.bits.expect(input_list(idx - 1).U)
          }
        }
      }

      // randomized test
      for (_ <- 0 until randTestLength) {
        input_list = Seq.fill(numInput)(r.nextInt(maxValue))
        sel_list = Seq.fill(numOutput)(r.nextInt(numInput+1))
        dut.vecInput.zip(input_list).foreach { case (input, value) =>
          input.bits.poke(value.U)
        }
        dut.sels.zip(sel_list) foreach { case (port, sel) =>
          port.poke(sel.U)
        }
        dut.vecOutput.zip(sel_list) foreach { case (output, sel) =>
          if (sel == 0) {
            output.bits.expect(0.U)
          } else {
            output.bits.expect(input_list(sel - 1).U)
          }
        }
      }
    }
  }
}
