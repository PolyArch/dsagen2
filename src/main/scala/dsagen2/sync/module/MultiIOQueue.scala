package dsagen2.sync.module

import chisel3._
import chisel3.util._
import dsagen2.misc.module.{MaskAggregator, MaskDistributor}
import dsagen2.util.UIntUtil.groupBitsAs

/**
 * Multiple Input/Output, First In First Out Queue, the input/output order is maintained
 *
 * This is the legacy version of AggDisMultiIOQueue (Aggregation Distribution Multi-IO Queue)
 * Please use [[AggDisMultiIOQueue]] instead
 *
 * @param numInput Number of Input Port
 * @param inputBits Data Bits for each input port
 * @param numOutput Number of output port
 * @param outputBits data bits of output port
 * @param depthInMinBits FIFO Depth in unit of Minimum Bits
 */
class MultiIOQueue(numInput : Int, inputBits : Int,
                   numOutput : Int, outputBits : Int,
                   val depthInMinBits : Int) extends MultiIOModule {
  /* -------------------------      Extract Parameters      ------------------------- */

  /* -------------------------     Derived Parameters       ------------------------- */

  // Final Depth which is power of 2, ceiling to the nearest power of 2 number as depth if give depth is not power of 2
  def finalDepth : Int = 1 << log2Ceil(depthInMinBits)

  // Minimum data bits, unitBits of this queue
  def unitBits : Int = inputBits min outputBits

  // Minimum depth of this queue in unit of minDataBits-bit
  def minDepth : Int = (numInput * inputBits + numOutput * outputBits) / unitBits

  // Depth in bits
  def depthBits : Int = finalDepth * unitBits

  // Output to Minimum Ratio
  def output2min : Int = outputBits / unitBits

  // Input to Minimum Bits Ratio
  def input2min : Int = inputBits / unitBits

  // Depth in output bits granularity
  def depthOutput : Int = depthBits / outputBits

  /* -------------------------      Parameters Sanity Check ------------------------- */

  // depth needs to be larger than minimum depth
  require(finalDepth >= minDepth, s"This Multi IO FIFO require at least $minDepth (unit is $unitBits-bit) as depth, " +
    s"but the given is $finalDepth. " +
    s"Input Bits = $inputBits, Output Bits = $outputBits. " +
    s"#input = $numInput, #output = $numOutput")

  // Depth bits needs to be multiple of minimum bits
  require(depthBits % unitBits == 0, s"Depth is $depthBits-bit, minimum data bits is $unitBits, " +
    s"it has to be the multiple of minimum data bits")

  // Depth of minimum bits should be power of 2
  require(isPow2(finalDepth), s"Finest granularity of queue is $unitBits-bit, which results in" +
    s"depth of minimum bits = $finalDepth, it needs to be power of 2")

  // Input Bits and Output Bits should be multiple relationship
  require(inputBits % outputBits == 0 || outputBits % inputBits == 0,
    s"Input Bits = $inputBits, Output Bits = $outputBits, they are not in multiple relationship")

  /* -------------------------         Input / Output       ------------------------- */

  // O(ready), Vector Input
  val vecInput : Vec[DecoupledIO[UInt]] = IO(Flipped(Vec(numInput, DecoupledIO(UInt(inputBits.W)))))

  // O, Vector Output
  val vecOutput : Vec[DecoupledIO[UInt]] = IO(Vec(numOutput, DecoupledIO(UInt(outputBits.W))))

  // O, Number of valid unit in queue
  val numValidUnit : UInt = IO(Output(UInt(log2Ceil(finalDepth + 1).W)))

  // O, Whether this Queue is full
  val full : Bool = IO(Output(Bool()))

  // I, repeat signal, if repeat is high, then head moving and invalidate will not happen
  val repeatDequeue : Bool = IO(Input(Bool()))

  // Dequeue and Enqueue Fire
  val deqFire : Bool = IO(Output(Bool()))
  val enqFire : Bool = IO(Output(Bool()))

  /* -------------------------     Registers and Modules    ------------------------- */

  // F, Register Array that holds the data
  val dataPipe : Vec[UInt] = RegInit(VecInit(Seq.fill(finalDepth)(0.U(unitBits.W))))

  // F, Bit Register Array that indicate the valid of data
  val validPipe : Vec[Bool] = RegInit(VecInit(Seq.fill(finalDepth)(false.B)))

  // F, Head Pointer
  val headPtr : UInt = RegInit(0.U(log2Ceil(finalDepth).W))

  // F, Tail Pointer
  val tailPtr : UInt = RegInit(0.U(log2Ceil(finalDepth).W))

  /* -------------------------             Wires            ------------------------- */

  // C, Wire that convert the data pipe into output side granularity
  val dataOutputPipe : Vec[UInt] = VecInit(dataPipe.grouped(output2min).map(VecInit(_).asUInt()).toSeq)
  val validOutputPipe : Vec[Bool] = VecInit(validPipe.grouped(output2min).map(VecInit(_).asUInt().andR()).toSeq)

  // C, Head Pointer to access data pipe in output side granularity
  val headOutputPtr : UInt = headPtr(headPtr.getWidth - 1, headPtr.getWidth - log2Ceil(depthOutput))

  // C, Aggregated Vector Input to lower position, keep data type
  val (aggInputData, aggInputValid) : (Seq[UInt], Seq[Bool])=
    MaskAggregator(VecInit(vecInput.map(_.bits)), VecInit(vecInput.map(_.valid)))

  // C, Wire that connect to input side valid and data and split it into minibits
  val enqDataInMin : Vec[UInt] = WireInit(VecInit(Seq.fill(numInput * input2min)(0.U(unitBits.W))))
  val enqValidInMin : Vec[Bool] = WireInit(VecInit(Seq.fill(numInput * input2min)(false.B)))

  // C, Wire that connect to the first numOutput * outputBits - bit data starting from headPtr
  val deqWindowData : Vec[UInt] = WireInit(VecInit(Seq.fill(numOutput)(0.U(outputBits.W))))
  val deqWindowValid : Vec[Bool] = WireInit(VecInit(Seq.fill(numOutput)(false.B)))

  // C, Wire that indicate dequeue fired
  val deqFiredWire : Bool = WireInit(false.B)

  // C, Wire that indicate enqueue fired
  val enqFiredWire : Bool = WireInit(false.B)

  // C, Wire that connect to the first numInput * input2min valid starting from tailPtr
  val enqWindowReady : Vec[Bool] = WireInit(VecInit(Seq.fill(numInput * input2min)(false.B)))

  // C, Distributed Dequeue Data by Ready
  val disDeqWindowData : Vec[UInt] =
    VecInit(MaskDistributor(deqWindowData, VecInit(vecOutput.map(_.ready)))._1)

  // C, Distributed Dequeue Data Valid by Ready
  val disDeqWindowValid : Vec[Bool] =
    VecInit(MaskDistributor(deqWindowValid, VecInit(vecOutput.map(_.ready)))._1)

  // C, Output Ready in Split by output2min ratio
  val deqReadyInMin : Vec[Bool] = WireInit(VecInit(Seq.fill(numOutput * output2min)(false.B)))

  // C, Head pointer increase delta
  val headPtrDelta : UInt = PopCount(deqReadyInMin)

  // C, Tail pointer increase delta
  val tailPtrDelta : UInt = PopCount(enqValidInMin)

  // C, Wire that indicate this queue if full
  val fullWire : Bool = validPipe.asUInt().andR()

  // C, Next Head Pointer
  val nextHeadPtr : UInt = {
    val nhp : UInt = WireInit(0.U(headPtr.getWidth.W))
    // TODO, non-power2 depth of minimum bits support
    nhp := headPtr + headPtrDelta
    nhp
  }

  // C, Wire indicate whether each minBits element is swept by head pointer (invalidate)
  val headSwept : Vec[Bool] = WireInit(VecInit(Seq.fill(finalDepth)(false.B)))

  // C, Wire indicate whether each minBits element is swept by tail pointer (validate, data rewrite)
  val tailSwept : Vec[Bool] = WireInit(VecInit(Seq.fill(finalDepth)(false.B)))

  // C, Next Tail Pointer
  val nextTailPtr : UInt = {
    val ntp : UInt = WireInit(0.U(tailPtr.getWidth.W))
    ntp := tailPtr + tailPtrDelta // TODO, non-power2 depth of minimum bits support
    ntp
  }

  // C, Wire indicate whether head and tail pointer wrap round
  val headRound : Bool = nextHeadPtr < headPtr
  val tailRound : Bool = nextTailPtr < tailPtr

  // C, Number of Valid unit in Queue
  val numElem : UInt = {
    val nE : UInt = WireInit(0.U(log2Ceil(finalDepth + 1).W))
    nE := Mux(fullWire, finalDepth.U, tailPtr - headPtr)
    nE
  }

  // C, All Ready
  val allReady : Bool = enqWindowReady.asUInt().andR()

  // C, All Valid
  val allValid : Bool = deqWindowValid.asUInt().andR()

  /* -------------------------     Combinational Logics     ------------------------- */

  // Calculate when dequeue operation happen
  deqFiredWire := VecInit(vecOutput.map(_.ready).zip(disDeqWindowValid).map{
    case (ready, valid) => Mux(ready, valid, true.B)
  }).asUInt().andR()

  // Calculate when enqueue operation happen
  enqFiredWire := VecInit(enqValidInMin.zip(enqWindowReady).map{
    case (valid, ready) => Mux(valid, ready, true.B) /* All valid get its ready*/
  }).asUInt().andR() && enqValidInMin.asUInt().orR() /* valid exist */

  // Connect deq Window Data and Valid
  for(outputIdx <- 0 until numOutput){
    val offsetOutputPtr : UInt = WireInit(0.U(headOutputPtr.getWidth.W))
    offsetOutputPtr := headOutputPtr + outputIdx.U(headOutputPtr.getWidth.W)
    deqWindowData(outputIdx) := dataOutputPipe(offsetOutputPtr)
    deqWindowValid(outputIdx) := validOutputPipe(offsetOutputPtr)
  }

  // Connect enq Window Ready
  for(inputIdx <- 0 until numInput * input2min){
    val offsetTailPtr : UInt = WireInit(0.U(tailPtr.getWidth.W))
    offsetTailPtr := tailPtr + inputIdx.U
    enqWindowReady(inputIdx) := !validPipe(offsetTailPtr)
  }

  // Connect Dequeue Ready Split by Outputbits to minimum bits ratio
  deqReadyInMin.zipWithIndex.foreach{
    case (ready, idx) => ready := vecOutput(idx / output2min).ready
  }

  // Connect enqueue valid windows by split input vector valid
  enqDataInMin.zip(enqValidInMin).zipWithIndex.foreach{
    case ((data, valid), idx) =>
      val inputIdx : Int = idx / input2min
      val groupIdx : Int = idx % input2min
      val dataBits : UInt = aggInputData(inputIdx)
      val dataGroup : Seq[UInt] = groupBitsAs(dataBits, unitBits)
      data := dataGroup(groupIdx)
      valid := aggInputValid(inputIdx)
  }

  // Calculate whether this element is swept by head pointer
  for(idx <- 0 until finalDepth){
    val geHead = idx.U >= headPtr
    val ltNext = idx.U < nextHeadPtr
    // If not round, then in between means swept, if rounded,
    headSwept(idx) := Mux(!headRound, geHead && ltNext, geHead || ltNext)
  }

  // Calculate whether this element is swept by tail pointer
  for(idx <- 0 until finalDepth){
    val geTail = idx.U >= tailPtr
    val ltTail = idx.U < nextTailPtr
    tailSwept(idx) := Mux(!tailRound, geTail && ltTail, geTail || ltTail)
  }

  /* -------------------------     Finite State Machine     ------------------------- */

  // Increase Head Pointer
  when(deqFiredWire && !repeatDequeue){headPtr := nextHeadPtr}

  // Increase Tail Pointer
  when(enqFiredWire){tailPtr := nextTailPtr}

  // Write Data Array if it is swept by tail pointer and enqInputValid is true
  for(idx <- 0 until finalDepth){
    val posAtEnq = idx.U - tailPtr
    require(dataPipe.head.getWidth == enqDataInMin.head.getWidth)
    when(tailSwept(idx) && enqFiredWire){
      dataPipe(idx) := enqDataInMin(posAtEnq)
    }
  }

  // Validate and Invalidate validArray
  for(idx <- 0 until finalDepth){
    val curr : Bool = validPipe(idx)
    validPipe(idx) :=
      Mux(enqFiredWire && tailSwept(idx), true.B, // if swept by tail and enqueue fired, validate
        Mux(deqFiredWire && !repeatDequeue && headSwept(idx), false.B, // not enqueue, but dequeue and swept by head, invalid
          curr) // keep current value
      )
  }

  /* -------------------------       Output Connection      ------------------------- */

  // Output Side Data
  require(vecOutput.length == disDeqWindowData.length)
  vecOutput.zip(disDeqWindowData).foreach{
    case (outPort, data) => outPort.bits := data
  }

  // Output Side Valid
  vecOutput.zipWithIndex.foreach{
    // If not all valid, will only be valid when fired
    case (outPort, idx) => outPort.valid :=
      // Fire only at ready
      Mux(allValid && outPort.ready, true.B,
        Mux(deqFiredWire && outPort.ready,
          disDeqWindowValid(idx),
          false.B
        )
      )
  }

  // Full
  full := fullWire

  // Number of valid unit
  numValidUnit := numElem

  // Input Ready, prepared for all valid input, so it needs to be all ready
  vecInput.foreach(inPort => inPort.ready := allReady || enqFiredWire)

  // Fire State
  deqFire := deqFiredWire
  enqFire := enqFiredWire

  /* -------------------------     Hardware Sanity Check    ------------------------- */

  /* -------------------------            Utility           ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */
}
