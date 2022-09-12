package dsagen2.sync.module

import chisel3._
import chisel3.util._
import dsagen2.mem.bundle.StreamState
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.sync.bundle.Pad1H
import dsagen2.util.UIntUtil.RegEnableThru

/**
 * Tag each compute vector with the state of stream
 *
 * TODO: this implementation is not finished before we realize we don't need it
 *
 * @param memUnitBits The minimum unit of memory, usually 8 means byte addressable memory
 * @param memWidth The max width of the memory engine connected to it
 * @param maxL1DBits The max bits needed to specify the length1D
 * @param numDataTypeExp How many kind of data type required by stream
 * @param padding Whether support padding
 * @param compWidth Max width on the compute side vector port
 * @param compBits The bit width for each compute port
 */
class StreamStateTagger(val memUnitBits : Int = 8, val memWidth : Int, val maxL1DBits : Int,
                        val numDataTypeExp : Int, val padding : Boolean,
                        val compWidth : Int, val compBits : Int) extends MultiIOModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  // Stream Depth, means how many 1D stream can be stored in Stream State Tagger
  val stateDepth : Int = 2

  /* ------------------------- Derived Parameters           ------------------------- */

  // Calculate the compute to memory exponential
  def comp2memExp : Int = log2Ceil(compBits / memUnitBits)

  /* ------------------------- Diplomatic Node              ------------------------- */
  /* ------------------------- Parameters Sanity Check      ------------------------- */
  /* ------------------------- Input / Output               ------------------------- */

  // Data Type of the current Stream, should be set at the total stream start and reset at the end of total stream
  val memExp : Option[UInt] = if(numDataTypeExp > 1) Some(IO(Input(UInt(log2Ceil(numDataTypeExp).W)))) else None

  // Padding Mode, padding at the start of stream does not make sense, should be deprecated
  val padMode : Option[UInt] =
    if(padding) Some(IO(Input(UInt(LINEAR_PADDING_BITS.W)))) else None

  // Dequeue repeat from outside
  val repeatDequeue : Bool = IO(Input(Bool()))

  // The memory mask from memory engine
  val memMask : ValidIO[UInt] = IO(Flipped(ValidIO(UInt(memWidth.W))))

  // The memory stream state, valid bit depends on memMask
  val strState : StreamState = IO(Input(new StreamState))

  // Compute Side Mask, indicate how many compute ports are active
  val compMask : UInt = IO(Input(UInt(compWidth.W)))

  // Output Stream State, output ready means next compute vector, can be overwritten by IVP repeat dequeue
  val vecState : DecoupledIO[StreamState] = IO(DecoupledIO(new StreamState))

  // Output Padding Control Signal to control input vector port dequeue
  val padCtrl : ValidIO[Pad1H] = IO(ValidIO(new Pad1H))

  /* ------------------------- Registers                    ------------------------- */

  // Buffer for Length1D+valid, and Stream State
  // Each position means a 1D stream
  val l1dValids      : Vec[Bool]        = RegInit(VecInit(Seq.fill(stateDepth)(false.B)))
  val l1dBuffer      : Vec[UInt]        = RegInit(VecInit(Seq.fill(stateDepth)(UInt(maxL1DBits.W))))
  val stateBuffer    : Vec[StreamState] = RegInit(VecInit(Seq.fill(stateDepth)(0.U.asTypeOf(new StreamState))))

  // Enqueue and Dequeue pointer to the length1D buffer, since it is double buffering, only 1bit is needed
  val tailPtr : Counter = Counter(stateDepth)
  val tail    : UInt = tailPtr.value
  val headPtr : Counter = Counter(stateDepth)
  val head    : UInt = headPtr.value

  // Wire, indicate a total new stream
  val newStream : Bool = memMask.valid && strState.StartStr

  // Register to hold the data type, it will not be changed during the lifetime of a stream
  val dataTypeExp : UInt = memExp match {case Some(value) => RegEnableThru(value, newStream);case None => 0.U}

  // Register to hold the padding info during the lifetime of stream
  val pad1h : Pad1H = decPadMode(padMode match { case Some(pMode) => RegEnableThru(pMode, newStream);case None => noPad})

  /* ------------------------- Wires / Combination Logic    ------------------------- */

  // The current valid, length1D and state
  val headValid : Bool        = l1dValids(head)
  val headL1D   : UInt        = l1dBuffer(head)
  val headState : StreamState = stateBuffer(head)

  // The next valid, length1D and state
  val tailValid : Bool        = l1dValids(tail)
  val tailL1D   : UInt        = l1dBuffer(tail)
  val tailState : StreamState = stateBuffer(tail)

  // Count the number of element required by compute side in unit of stream elements
  val numCompNeed: UInt = ((PopCount(compMask)  << comp2memExp) >> dataTypeExp).asUInt()

  // Count the number of element fed by memory side in unit of stream element
  val numMemFeed : UInt = (PopCount(memMask.bits) >> dataTypeExp).asUInt()

  // Whether we reach to the end of current 1D stream
  // Cross to next will trigger the state from next 1D
  // If we want to cross to next 1D stream, the current left must be less and not padding 1D
  val cross2NextL1D : Bool = headL1D < numCompNeed && !pad1h.pad1D
  val justFit       : Bool = headL1D === numCompNeed
  val end1D         : Bool = headState.End1D && cross2NextL1D || justFit

  // The higher dimension state go through gate of lower dimension state
  val start2D : Bool = headState.Start1D && headState.Start2D
  val end2D   : Bool = end1D && headState.End2D
  val startStr: Bool = start2D && headState.StartStr
  val endStr  : Bool = end2D && headState.EndStr

  // The moment that the current compute vector is fired from vector port
  // Current L1D is valid and
  val fired : Bool = headValid && vecState.ready

  // The moment of moving to next Length1D state, basically fired and not repeatDequeue
  val move2NextVec : Bool = fired && !repeatDequeue

  // The moment to move to next Length1D from queue
  val move2NextL1D : Bool = move2NextVec && end1D

  // The state for each position of buffer: head-only, tail-only, head-tail
  // Position for each buffer to update
  class queueState extends Bundle{
    val isHead : Bool = Bool()
    val isTail : Bool = Bool()
    val nxtHead : Bool = Bool()
  }
  val queueStates : Vec[queueState] = WireInit(VecInit(Seq.fill(stateDepth)(0.U.asTypeOf(new queueState))))
  for(idx <- 0 until stateDepth){
    queueStates(idx).isHead := head === idx.U
    queueStates(idx).isTail := tail === idx.U
    queueStates(idx).nxtHead := head === (idx-1).U
  }

  // The next Length1D for head/tail/head+1 position
  val nxtHeadL1D : UInt = headL1D - numCompNeed
  val nxtTailL1D : UInt = tailL1D + numMemFeed
  val nxtHeadTailL1D : UInt = nxtHeadL1D + numMemFeed
  val l1dLeftover : UInt = l1dBuffer(head + 1.U) - (numCompNeed - headL1D)

  // The next state for head/tail/head+1 position, since all higher dimension is gated by lower dimension
  // and end of 1D is calculated

  /* ------------------------- Finite State Machine         ------------------------- */

  // FSM for each position of the buffer
  for(idx <- 0 until stateDepth){
    // Get the register
    val valid : Bool = l1dValids(idx)
    // Get the judgement state
    val isHead : Bool = queueStates(idx).isHead
    val isTail : Bool = queueStates(idx).isTail
    val nxtHead: Bool = queueStates(idx).nxtHead
    when(isHead && isTail){
      // This position is head and tail
      when(valid){
        // this position is valid now, it can be dequeued, enqueued and de-enqueued
        when(move2NextVec && memMask.valid){
          // This position is indeed be enqueued and dequeued at the same time
          l1dBuffer(idx) := nxtHeadTailL1D
        }.elsewhen(move2NextVec){

        }.elsewhen(memMask.valid){

        }
      }.otherwise{

      }
    }.elsewhen(isTail){

    }.elsewhen(isHead){

    }.elsewhen(nxtHead) {

    }.otherwise{

    }
  }

  // FSM for head and tail pointer

  /* ------------------------- Output Connection            ------------------------- */

  // Valid
  vecState.valid := headValid
  padCtrl.valid := headValid

  // Stream State
  vecState.bits.Start1D := headState.Start1D || cross2NextL1D
  vecState.bits.End1D := end1D
  vecState.bits.Start2D := start2D
  vecState.bits.End2D := end2D
  vecState.bits.StartStr := startStr
  vecState.bits.EndStr := endStr

  // Padding Control Signal
  padCtrl.bits := pad1h

  /* ------------------------- Hardware Sanity Check        ------------------------- */
  /* ------------------------- Post Generation Sanity Check ------------------------- */
  /* ------------------------- Utility                      ------------------------- */

  def decPadMode(pad: UInt): Pad1H = {
    val pad1h: Pad1H = WireInit(0.U.asTypeOf(new Pad1H))
    pad1h.noPad := pad === noPad
    pad1h.padZeroStrEnd := pad === padZeroStrEnd
    pad1h.padOffStrEnd := pad === padOffStrEnd
    pad1h.padZero2DEnd := pad === padZero2DEnd
    pad1h.padOff2DEnd := pad === padOff2DEnd
    pad1h.padZero1DEnd := pad === padZero1DEnd
    pad1h.padOff1DEnd := pad === padOff1DEnd
    pad1h
  }
}