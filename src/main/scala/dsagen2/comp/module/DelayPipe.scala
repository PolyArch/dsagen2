package dsagen2.comp.module

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.internal.requireIsChiselType
import chisel3.util._

/** A pipe with reconfigurable delay cycle, delay ==0 means input value will appear at output at the same cycle
  *
  * @param maxDelay 0 <= delay cycle <= maxDelay, delay 0 means combinational logics
  */
class DelayPipe[T <: Data](val gen: T, maxDelay: Int) extends MultiIOModule {
  require(maxDelay >= 0, s"Max Delay Amount cannot be negative, but it is $maxDelay now")
  val genType: T = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  // IO
  val input:  ValidIO[T] = IO(Flipped(ValidIO(genType)))
  val output: ValidIO[T] = IO(ValidIO(genType))
  val delay:  UInt = if (maxDelay > 0) IO(Input(UInt(log2Ceil(maxDelay + 1).W))) else IO(Input(UInt(1.W)))
  val busy:   Bool = IO(Output(Bool()))

  if (maxDelay > 0) {
    // Register and Wire
    val dataExist: Bool = WireInit(false.B)
    val tailPtr:   UInt = if (maxDelay > 0) WireDefault(0.U(log2Ceil(maxDelay + 1).W)) else 0.U
    val pipe_bit: Vec[T] =
      RegInit(VecInit(Seq.fill(maxDelay + 1)(0.U.asTypeOf(genType))))
    val pipe_valid: Vec[Bool] =
      RegInit(VecInit(Seq.fill(maxDelay + 1)(false.B)))
    val headPtr: UInt = if (maxDelay > 0) RegInit(0.U(log2Ceil(maxDelay + 1).W)) else 0.U

    // Combinational Logics
    dataExist := pipe_valid.asUInt().orR() || input.valid
    tailPtr := {
      val nextTail: UInt = headPtr + delay
      if (isPow2(maxDelay + 1)) {
        nextTail
      } else {
        Mux(nextTail > maxDelay.U, nextTail - maxDelay.U, nextTail)
      }
    }

    // FSM
    // Write data into pipe
    when(delay =/= 0.U && input.valid) {
      pipe_bit(tailPtr) := input.bits
      pipe_valid(tailPtr) := true.B
    }
    // Move the head pointer
    when(dataExist && delay =/= 0.U) {
      // reset valid bit
      pipe_valid(headPtr) := false.B
      // Increase head pointer
      headPtr := {
        val nextHead: UInt = headPtr + 1.U
        if (isPow2(maxDelay + 1)) {
          nextHead
        } else {
          Mux(nextHead > maxDelay.U, nextHead - maxDelay.U, nextHead)
        }
      }
    }

    // Output Connection + bypass when delay = 0
    output.bits := Mux(delay === 0.U, input.bits, pipe_bit(headPtr))
    output.valid := Mux(delay === 0.U, input.valid, pipe_valid(headPtr))
    busy := dataExist
  } else {
    output <> input // Static Delay = 0, no delay applied
  }
}
