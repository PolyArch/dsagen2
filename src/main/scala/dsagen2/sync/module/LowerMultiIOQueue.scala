package dsagen2.sync.module

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import dsagen2.util.UIntUtil._

/** A simple multiple IO queue that can handle different data type
  *
  * Features:
  *  Multiple IO: Input side and Output side can have different number of ports, enqueue and dequeue only happens from
  *    lowest part of of the vector port. It will ignore tha higher port of request if it is not connected to lower part
  *  Different Data Type: Input bits and output bits can be different, but should be in multiple relationship
  *  Depth is in the unit of minimum bits between input bits and output bits, which can only be suggested. If the
  *    suggested depth is shallower than minimum depth, than warning will be printed and minimum depth is used
  */
class LowerMultiIOQueue(
  val numInput:   Int,
  val inputBits:  Int,
  val numOutput:  Int,
  val outputBits: Int,
  val depthByte:  Int,
  val supportPad: Boolean = false,
  val padWidth:   Int = -1)
                       (implicit p: Parameters)
    extends MultiIOModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  // Suggest name
  suggestName(s"lowerQueue_i${numInput}x${inputBits}_o${numOutput}x${outputBits}")

  /* ------------------------- Derived Parameters           ------------------------- */
  // Calculate the actual depth by ceiling the suggested depth to nearest power of 2 number
  // TODO: we can certainly support a non-power-of-2 depth in the future
  def depth: Int = 1 << log2Ceil(depthByte)

  // Minimum data bits, unitBits of this queue
  def unitBits: Int = inputBits.min(outputBits)

  // Minimum depth of this queue in unit of minDataBits-bit
  def minDepth: Int = 1 << log2Ceil((numInput * input2min).max(numOutput * output2min))

  // Depth in bits
  def depthBits: Int = depth * unitBits

  // Output to Minimum Ratio
  def output2min: Int = outputBits / unitBits

  // Input to Minimum Bits Ratio
  def input2min: Int = inputBits / unitBits

  // Depth in output side data granularity
  def depthOutput: Int = depthBits / outputBits

  /* ------------------------- Diplomatic Node              ------------------------- */
  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // Input and Output Bits should be in multiple relationship
  // depth needs to be larger than minimum depth
  require(
    depth >= minDepth,
    s"This Multi IO FIFO require at least $minDepth (unit is $unitBits-bit) as depth, " +
      s"but the given is $depthByte. " +
      s"Input Bits = $inputBits, Output Bits = $outputBits. " +
      s"#input = $numInput, #output = $numOutput"
  )

  // Depth bits needs to be multiple of minimum bits
  require(
    depthBits % unitBits == 0,
    s"Depth is $depthBits-bit, minimum data bits is $unitBits, " +
      s"it has to be the multiple of minimum data bits"
  )

  // Depth of minimum bits should be power of 2
  require(
    isPow2(depth),
    s"Finest granularity of queue is $unitBits-bit, which results in" +
      s"depth of minimum bits = $depth, it needs to be power of 2"
  )

  // Input Bits and Output Bits should be multiple relationship
  require(
    inputBits % outputBits == 0 || outputBits % inputBits == 0,
    s"Input Bits = $inputBits, Output Bits = $outputBits, they are not in multiple relationship"
  )

  // Padding Width check
  if(!supportPad) require(padWidth == -1, s"Padding width should be -1 if no padding supported")
    // TODO: padding width does not need to be power of 2
  else require(requirement = true/*isPow2(padWidth)*/, s"Padding width should be power of 2, but it is $padWidth")

  /* ------------------------- Input / Output               ------------------------- */

  // Input Vector Port
  val vecInput: Vec[DecoupledIO[UInt]] = IO(Flipped(Vec(numInput, DecoupledIO(UInt(inputBits.W)))))

  // Input Synchronized, if any one of this is true then we need to wait for all synchronized input be valid
  // before firing them into queue
  val inputSyncs: Vec[Bool] = IO(Input(Vec(numInput, Bool())))

  // Output Vector Port
  val vecOutput: Vec[DecoupledIO[UInt]] = IO(Vec(numOutput, DecoupledIO(UInt(outputBits.W))))

  // Output Synchronization like above
  val outputSyncs: Vec[Bool] = IO(Input(Vec(numOutput, Bool())))

  // Number of remaining element, the current number of element in the queue
  val count: UInt = IO(Output(UInt(log2Ceil(depth + 1).W)))

  // Number of valid element, the current number of available space in the queue
  val left: UInt = IO(Output(UInt(log2Ceil(depth + 1).W)))

  // Fullness signal
  val full: Bool = IO(Output(Bool()))

  // Empty signal
  val empty: Bool = IO(Output(Bool()))

  // Enqueue Fired
  val enqFire: Bool = IO(Output(Bool()))

  // Dequeue Fired
  val deqFire: Bool = IO(Output(Bool()))

  // Repeat dequeue (do not change the header pointer)
  val repeatDequeue: Bool = IO(Input(Bool()))

  // Padding Control
  val padZero : Option[Bool] = if(supportPad) Some(IO(Input(Bool()))) else None
  val padOff  : Option[Bool] = if(supportPad) Some(IO(Input(Bool()))) else None
  val numPadLeft : Option[UInt] = if(supportPad) Some(IO(Input(UInt(log2Ceil(padWidth).W)))) else None

  /* -------------------------     Registers and Modules    ------------------------- */

  // Memory Block based queue
  val blockQueue: MemBlockMultiIOQueue[UInt] = Module(
    new MemBlockMultiIOQueue[UInt](
      UInt(unitBits.W),
      numInput = numInput * input2min,
      numOutput = numOutput * output2min,
      inputSync = true,
      outputSync = true,
      depth = {
        if (depth % minDepth == 0) depth
        else (depth / minDepth + 1) * minDepth // ceiling to nearest multiple
      },
      supportPad = supportPad,
      padWidth = padWidth
    )
  )

  /* ------------------------- Combination Logic            ------------------------- */

  // Enqueue Side Connection
  require(vecInput.length == blockQueue.vecInput.length / input2min)
  val queueGroupInSync: Option[Seq[Seq[Bool]]] = blockQueue.vecInputSync match {
    case Some(value) => Some(value.grouped(input2min).toSeq)
    case None        => None
  }
  vecInput.zip(inputSyncs).zip(blockQueue.vecInput.grouped(input2min).toSeq).zipWithIndex.foreach {
    case (((in, inSync), seqIn), idx) =>
      // Bits and Valid
      require(in.bits.getWidth == seqIn.map(_.bits.getWidth).sum)
      val groupBits: Seq[UInt] = groupBitsAs(in.bits, unitBits)
      require(groupBits.length == seqIn.length)
      seqIn.zip(groupBits).foreach { case (iIn, iBits) =>
        require(iIn.bits.getWidth == iBits.getWidth)
        iIn.bits := iBits
        iIn.valid := in.valid
      }
      // Ready
      in.ready := VecInit(seqIn.map(_.ready)).asUInt().andR()
      // Sync
      queueGroupInSync match {
        case Some(value) => value(idx).foreach(s => s := inSync)
        case None        =>
      }
  }

  // Dequeue Side Connection
  require(vecOutput.length == blockQueue.vecOutput.length / output2min)
  val queueGroupOutSync: Option[Seq[Seq[Bool]]] = blockQueue.vecOutputSync match {
    case Some(value) => Some(value.grouped(output2min).toSeq)
    case None        => None
  }
  vecOutput.zip(outputSyncs).zip(blockQueue.vecOutput.grouped(output2min).toSeq).zipWithIndex.foreach {
    case (((out, outSync), seqOut), idx) =>
      // Bits and valid and Ready
      require(
        out.bits.getWidth == VecInit(seqOut.map(_.bits)).asUInt().getWidth,
        s"out.bits = ${out.bits.getWidth}-bit, seqOut.bits.sum = ${VecInit(seqOut.map(_.bits)).asUInt().getWidth}-bit"
      )
      out.bits := VecInit(seqOut.map(_.bits)).asUInt()
      out.valid := VecInit(seqOut.map(_.valid)).asUInt().andR()
      seqOut.foreach(s => s.ready := out.ready)
      // Sync
      queueGroupOutSync match {
        case Some(value) => value(idx).foreach { s => s := outSync }
        case None        =>
      }
  }

  // Connect padding
  if(supportPad){
    blockQueue.padZero.get := padZero.get
    blockQueue.padOff.get := padOff.get
    blockQueue.numPadLeft.get := numPadLeft.get
  }

  // Output statistic
  count := blockQueue.count
  left := blockQueue.left

  // Empty / Full
  full := blockQueue.full
  empty := blockQueue.empty

  // Enqueue / Dequeue Fired
  enqFire := blockQueue.enqFire
  deqFire := blockQueue.deqFire

  // Dequeue Repeat
  blockQueue.repeatDequeue := repeatDequeue
}
