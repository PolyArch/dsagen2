package dsagen2.sync.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.StreamState
import dsagen2.util.{QueueFPGA, WithQueueIO}

class StreamStateQueue(maxVec: Int, vecWidth : Int)(implicit val p: Parameters) extends MultiIOModule {
  /* ------------------------- Extract Parameters           ------------------------- */
  suggestName(s"stateQueue_d${maxVec+1}_w$vecWidth")

  /* ------------------------- Derived Parameters           ------------------------- */

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  /* ------------------------- Input / Output               ------------------------- */

  // Input Side
  val enqState : DecoupledIO[StreamState] = IO(Flipped(DecoupledIO(new StreamState)))
  val enqNumVec : UInt = IO(Input(UInt(log2Ceil(maxVec + 1).W)))
  val enqNumLeft : UInt = IO(Input(UInt(log2Ceil(vecWidth).W))) // The remaind of element per vector, never be vecWidth
  // Output Side
  val deqState : DecoupledIO[StreamState] = IO(DecoupledIO(new StreamState))
  val deqNumLeft : UInt = IO(Output(UInt(log2Ceil(vecWidth).W)))

  /* ------------------------- Registers                    ------------------------- */

  // Indicate whether the head of queue is start of stream
  val isStart : Bool = RegInit(true.B)

  // Indicate the remaining vector that head of stream will cover
  val numRemainVec : UInt = RegInit(0.U(log2Ceil(maxVec + 1).W))

  /* ------------------------- Modules                      ------------------------- */

  // TODO: I actually don't know what will be the proper depth for queue, for now I just use maxVec + 1
  //  maybe we can reduce it?

  // Queue to hold the Stream State
  val stateQueue : WithQueueIO[StreamState] = Module(new QueueFPGA(new StreamState, maxVec + 1, p(FPGAOverlay)))

  // Queue to count down the covered vector
  val countQueue : WithQueueIO[UInt] = Module(new QueueFPGA(UInt(log2Ceil(maxVec + 1).W), maxVec + 1, p(FPGAOverlay)))

  // Queue to hold the leftover number of element
  val leftQueue : WithQueueIO[UInt] = Module(new QueueFPGA(UInt(log2Ceil(vecWidth + 1).W), maxVec + 1, p(FPGAOverlay)))

  /* ------------------------- Wires                        ------------------------- */

  // Initial count from count queue
  val initCount : UInt = countQueue.io.deq.bits

  // The end of covered vector for the head state based on queue info
  val isEndFromQueue : Bool = initCount === 1.U && countQueue.io.deq.valid

  // The end of covered vector for the head state based on remaining vector register
  val isEndFromCount : Bool = numRemainVec === 1.U && countQueue.io.deq.valid

  // Reach the end of covered state
  val isEnd : Bool = isEndFromCount || isEndFromQueue

  /* ------------------------- Combination Logic            ------------------------- */

  // The enqueue side is not special
  stateQueue.io.enq <> enqState
  countQueue.io.enq.bits := enqNumVec
  countQueue.io.enq.valid := enqState.valid
  leftQueue.io.enq.bits := enqNumLeft
  leftQueue.io.enq.valid := enqState.valid

  // Dequeue the state queue and count queue when reach the last vector
  stateQueue.io.deq.ready := deqState.ready && isEnd
  countQueue.io.deq.ready := deqState.ready && isEnd
  leftQueue.io.deq.ready := deqState.ready && isEnd

  /* ------------------------- Finite State Machine         ------------------------- */

  // Change the Start State
  when(deqState.fire()){
    when(isEnd){isStart := true.B}.otherwise{isStart := false.B}
  }

  // Refresh the number of remain counter
  when(isStart && countQueue.io.deq.valid){
    // If it is in start state
    when(deqState.ready){
      // if downstream is doing fresh at the same time, assign with initCount-1
      numRemainVec := Mux(initCount =/= 0.U, initCount - 1.U, 0.U)
    }.elsewhen(!isEnd){
      // And downstream is not ready yet, then refresh it
      numRemainVec := initCount
    }
  }.elsewhen(deqState.ready && numRemainVec =/= 0.U){
    // Decrease count when dequeue
    numRemainVec := numRemainVec - 1.U
  }

  /* ------------------------- Output Connection            ------------------------- */

  deqState.bits := stateQueue.io.deq.bits.mask(isStart, isEnd)
  deqState.valid := stateQueue.io.deq.valid
  deqNumLeft := leftQueue.io.deq.bits

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  /* ------------------------- Utility                      ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */

}
