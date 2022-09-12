package dsagen2.sync.module

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.util.AppUtil.{bitvecSynchronized, gen2genType}
import dsagen2.mem.module.bank.DualPort1w1rSyncReadRAM
import dsagen2.top.config.DSAFixedConfig.MAX_VP_BYTE
import dsagen2.util.UIntUtil.groupBitsAs

/** Multi-IO Queue, same datatype for both input and output side, SyncReadMem is used to implement this Queue.
 * Number of both side port should be power of 2, depth should also be power of two
 */
class MemBlockMultiIOQueue[T <: Data](
                                       gen:            T,
                                       val numInput:   Int,
                                       val numOutput:  Int,
                                       val inputSync:  Boolean,
                                       val outputSync: Boolean,
                                       val depth:      Int,
                                       val supportPad: Boolean = false,
                                       val padWidth:   Int = -1)
                                     (implicit p: Parameters)
  extends MultiIOModule {
  /* ------------------------- Derived Parameters           ------------------------- */
  // Suggest Name
  suggestName(
    s"blockQueue" +
      s"_nI${numInput}_nO$numOutput" +
      s"_d${depthPerBlock}x$numBlock" +
      s"_w${genType.getWidth}"
  )

  // Parameterized the type
  def genType: T = gen2genType(gen, compileOptions)

  // Number of memory block for storing data
  // Since the LSBs are used as block selection, so the number of block has to be power of 2
  // If the number of block is not power of two, then we ceiling to nearest power of 2
  def numBlock: Int = 1 << log2Ceil(numInput.max(numOutput))

  // Number of bits for selecting block
  def blockBits: Int = if (numBlock > 1) log2Ceil(numBlock) else 0

  // Depth per block
  def depthPerBlock: Int = depth / numBlock

  // Number of bits needed for per block depth
  def depthBits: Int = if (depthPerBlock > 1) log2Ceil(depthPerBlock) else 0

  // Number of bits needed for each port to enqueue or dequeue to the block
  def addrBits: Int = depthBits + blockBits

  // Number of unit per pad
  def numUnitPerPad: Int = numOutput / padWidth

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // Must have one block
  require(
    numInput > 0 && numOutput > 0 && isPow2(numBlock) && isPow2(depthPerBlock),
    s"#Input = $numInput, #Output = $numOutput: " +
      s"depthPerBlock = $depthPerBlock, numBlock = $numBlock, should be both power of 2 and positive"
  )

  // Depth
  require(
    depth >= numBlock && depth % numBlock == 0 && isPow2(depth),
    s"Depth = $depth need to be multiple of max(numInput, numOutput) = $numBlock and power of 2"
  )

  // Depth per block should be at least two, we should not use single register as vector port
  require(depthPerBlock >= 2, s"The *vector depth* of vector port is just $depthPerBlock, at least 2 is needed")

  // Padding Width check
  if (!supportPad) require(padWidth == -1, s"Padding width should be -1 if no padding supported")
  else require(requirement = true, s"Padding width should be power of 2, but it is $padWidth")

  // Total depth cannot exceed max
  require(numBlock * depthPerBlock <= MAX_VP_BYTE,
    s"VP configured to be ${numBlock * depthPerBlock}, but only $MAX_VP_BYTE allowed")

  /* ------------------------- Input / Output               ------------------------- */

  // Input Vector Port
  val vecInput: Vec[DecoupledIO[T]] = IO(Flipped(Vec(numInput, DecoupledIO(genType))))

  // Input Synchronized
  // If any one of this is true then we need to wait for all synchronized input be valid before firing them into queue
  val vecInputSync: Option[Vec[Bool]] = if (inputSync) Some(IO(Input(Vec(numInput, Bool())))) else None

  // Output Vector Port
  val vecOutput: Vec[DecoupledIO[T]] = IO(Vec(numOutput, DecoupledIO(genType)))

  // Output Synchronization like above
  val vecOutputSync: Option[Vec[Bool]] = if (outputSync) Some(IO(Input(Vec(numOutput, Bool())))) else None

  // Number of remaining element, the current number of element in the queue
  val count: UInt = IO(Output(UInt(log2Ceil(depth + 1).W)))
  val left:  UInt = IO(Output(UInt(log2Ceil(depth + 1).W)))

  // Fullness signal
  val full: Bool = IO(Output(Bool()))

  // Empty signal
  val empty: Bool = IO(Output(Bool()))

  // Enqueue Fired
  val enqFire: Bool = IO(Output(Bool()))

  // Dequeue Fired
  val deqFire: Bool = IO(Output(Bool()))

  // Repeat dequeue (do not change the header pointer and not invalid the data)
  val repeatDequeue: Bool = IO(Input(Bool()))

  // Padding Control
  val padZero:    Option[Bool] = if (supportPad) Some(IO(Input(Bool()))) else None
  val padOff:     Option[Bool] = if (supportPad) Some(IO(Input(Bool()))) else None
  val numPadLeft: Option[UInt] = if (supportPad) Some(IO(Input(UInt(log2Ceil(padWidth).W)))) else None

  /* -------------------------     Registers and Modules    ------------------------- */

  // The Sync Read Memory Blocks, Read-Under-Write undefined for backend purpose
  //val seqMemBlock: Seq[SyncReadMem[T]] = Seq.fill(numBlock)(SyncReadMem(depthPerBlock, genType, SyncReadMem.Undefined))
  val seqMemBlock: Seq[DualPort1w1rSyncReadRAM] =
    Seq.fill(numBlock)(Module(new DualPort1w1rSyncReadRAM(
      unitBits = genType.getWidth,
      widthBits = genType.getWidth,
      depth = depthPerBlock,
      maskWrite = false,
      latency = 1,
      isFPGA = false
  )))

  // Head and Tail Pointer Register
  val headPtrReg: Option[UInt] = if (addrBits > 0) Some(RegInit(0.U(addrBits.W))) else None
  val tailPtrReg: Option[UInt] = if (addrBits > 0) Some(RegInit(0.U(addrBits.W))) else None

  /* ------------------------- Wires                        ------------------------- */

  // Wire that indicate whether head pointer can move to next one
  val moveHead: Bool = WireInit(false.B)

  // Fire by padding request, without considering the number of remaining elements
  val fireByPadRequest : Bool = WireInit(false.B)

  // Fire by padding wire
  val deqFireByPadWire : Bool = WireInit(false.B)

  // Head and Tail Pointer
  val tailPtr: UInt = tailPtrReg.getOrElse(0.U(1.W))
  val headPtr: UInt = headPtrReg.getOrElse(0.U(1.W))

  // Register to record the dequeue fired state
  val deqFireReg: Bool = RegInit(false.B) // Since we use SyncReadMem, so RegInit

  // Wire, how many elements are needed for dequeue (delta to head pointer)
  val deltaHead: UInt = WireInit(0.U(log2Ceil(numOutput + 1).W))

  // Statistic of queue
  val numElemRaw: UInt = Mux(tailPtr >= headPtr, tailPtr - headPtr, depth.U - (headPtr - tailPtr))
  // The calculation of number of element in queue is pessimistic,
  // if it is being dequeued AND not repeat by vector, it will minus the needed
  // if it is doing repeating, than just the old one
  val numElem:  UInt = Mux(
    (deqFireReg || fireByPadRequest) && !repeatDequeue, Mux(numElemRaw > deltaHead, numElemRaw - deltaHead, 0.U), numElemRaw)
  val numSpace: UInt = depth.U - numElemRaw - 1.U // Available space is 1 less, can never be actually full

  // Full and Empty
  val fullWire:  Bool = numSpace <= 2.U // Remaining Byte is less than two
  val emptyWire: Bool = numElem === 0.U

  // Write wire for each memory block, write address, write data, write enable
  val seqWriteData: Seq[T] = Seq.fill(numBlock)(WireInit(0.U.asTypeOf(genType)))
  val seqWriteAddr: Seq[UInt] =
    if (depthBits > 0) Seq.fill(numBlock)(WireInit(0.U(depthBits.W))) else Seq.fill(numBlock)(0.U(1.W))
  val seqWriteEnable: Seq[Bool] = Seq.fill(numBlock)(WireInit(false.B))

  // Read wire for each memory block: read address, read data, read enable
  val seqReadData: Vec[T] = WireInit(VecInit(Seq.fill(numBlock)(0.U.asTypeOf(genType))))
  val seqReadAddr: Seq[UInt] =
    if (depthBits > 0) Seq.fill(numBlock)(WireInit(0.U(depthBits.W))) else Seq.fill(numBlock)(0.U(1.W))
  val seqReadEnable: Seq[Bool] = Seq.fill(numBlock)(WireInit(false.B))

  // Synchronized Vector Input Valid and Output Ready
  val vecInputValid:  Seq[Bool] = bitvecSynchronized(vecInput.map(_.valid), vecInputSync)
  val vecOutputReady: Seq[Bool] = bitvecSynchronized(vecOutput.map(_.ready), vecOutputSync)

  // Valid Input or Ready Output Exist
  val validInputExist:  Bool = VecInit(vecInputValid).asUInt().orR()
  val inputAllValid:    Bool = VecInit(vecInputValid).asUInt().andR()
  val readyOutputExist: Bool = VecInit(vecOutputReady).asUInt().orR()
  val outputAllReady:   Bool = VecInit(vecOutputReady).asUInt().andR()

  // Calculate delta of head and tail pointer
  val deltaTail: UInt =
    Mux(inputAllValid, numInput.U, Mux(!validInputExist, 0.U, PopCount(vecInputValid)))
  // Delta to head pointer by counting the element needed from output side
  val compDeltaHead: UInt =
    Mux(outputAllReady, numOutput.U, Mux(!readyOutputExist, 0.U, PriorityEncoder(vecOutputReady.map(!_))))
  require(compDeltaHead.getWidth <= deltaHead.getWidth)
  // Delta to head pointer when padded
  val padDeltaHead: Option[UInt] = if(supportPad) Some((numPadLeft.get << log2Ceil(numUnitPerPad)).asUInt()) else None

  // Whether enqueue have enough space: more than enough || enough for this request
  val canEnqBySpace: Bool = numSpace >= numInput.U || deltaTail <= numSpace
  val canDeqByElem4Regular:  Bool = numElem >= numOutput.U || compDeltaHead <= numElem
  val canDeqByElem4Pad:      Bool = numElem >= numOutput.U || padDeltaHead.getOrElse(0.U) <= numElemRaw

  // Next Head and Tail Pointer
  val nextTailPtr:   UInt = WireInit(0.U(tailPtr.getWidth.W))
  val rawNxtTailPtr: UInt = WireInit(0.U((tailPtr.getWidth + 1).W))
  nextTailPtr := Mux(rawNxtTailPtr >= depth.U, rawNxtTailPtr - depth.U, rawNxtTailPtr)
  val nextHeadPtr:   UInt = WireInit(0.U(headPtr.getWidth.W))
  val rawNxtHeadPtr: UInt = headPtr +& Mux(deltaHead > numElemRaw, numElemRaw, deltaHead)
  nextHeadPtr := Mux(rawNxtHeadPtr >= depth.U, rawNxtHeadPtr - depth.U, rawNxtHeadPtr)

  // When move head is true, the actual head pointer should be next one
  val actualHeadPtr: UInt = Mux(moveHead, nextHeadPtr, headPtr)

  val seqDeqAddr: Seq[UInt] = (0 until numOutput).map { idx =>
    val rawAddr: UInt = actualHeadPtr + idx.U
    if (isPow2(depth)) rawAddr
    else Mux(rawAddr >= depth.U, rawAddr - depth.U, rawAddr)
  }.map(x => x.apply(headPtr.getWidth - 1, 0))

  // Extract Block Selection and Depth (actual address to block) Selection from address
  val seqDeqBlockSel: Seq[UInt] = seqDeqAddr.map { addr => if (blockBits > 0) addr(blockBits - 1, 0) else 0.U(1.W) }
  val seqDeqDepthSel: Seq[UInt] = seqDeqAddr.map { addr =>
    if (depthBits > 0) addr(addrBits - 1, blockBits) else 0.U(1.W)
  }

  // Internal Dequeue Fire Wire (one cycle before deqFire output)
  val enqFireWire:      Bool = canEnqBySpace && validInputExist
  val deqFireWire:      Bool = canDeqByElem4Regular && readyOutputExist && !emptyWire
  val pad:              Bool = padOff.getOrElse(false.B) || padZero.getOrElse(false.B)
  fireByPadRequest := pad && readyOutputExist
  deqFireByPadWire := pad && readyOutputExist && canDeqByElem4Pad

  // Calculate the final delta head by combining the delta from needed and delta from padding
  deltaHead := Mux(pad, padDeltaHead.getOrElse(0.U), compDeltaHead)

  /* ------------------------- Combination Logic            ------------------------- */

  // Connect move head pointer wire
  moveHead := (deqFireReg || deqFireByPadWire) && readyOutputExist && !repeatDequeue

  // Circular Shift mask
  val totalWriteMask: UInt = WireInit(0.U((numBlock*2).W))
  totalWriteMask := (VecInit(vecInputValid).asUInt() << tailPtr(blockBits-1,0)).asUInt()
  val writeMask: UInt = totalWriteMask(numBlock*2-1, numBlock) | totalWriteMask(numBlock-1, 0)

  // Circular Shift data
  val totalWriteData: Vec[UInt] = Wire(Vec(numBlock*2, UInt(genType.getWidth.W)))
  totalWriteData.zipWithIndex.foreach { case (word, idx) =>
    val data: Seq[T] = vecInput.map(_.bits)
    val mapping: Seq[(UInt, Data)] =
      for (shift_by <- data.indices) yield {
        if (shift_by <= idx && idx-shift_by < data.length && idx-shift_by >= 0) {
          (shift_by.U, data(idx-shift_by))
        } else {
          (shift_by.U, 0.U)
        }
      }
    word := MuxLookup(tailPtr(blockBits-1, 0), 0.U, mapping)
  }
  val writeData: Seq[UInt] = {
    val dataBitWidth: Int = numBlock*genType.getWidth
    groupBitsAs(totalWriteData.asUInt()(dataBitWidth*2-1, dataBitWidth) |
      totalWriteData.asUInt()(dataBitWidth-1, 0), genType.getWidth)
  }

  seqWriteAddr.zip(seqWriteEnable).zip(seqWriteData).zipWithIndex
    .foreach { case (((addr, enable), data), idx) =>
      enable := writeMask(idx) && canEnqBySpace
      data := writeData(idx)
      when(idx.U < tailPtr(blockBits - 1, 0)) {
        addr := tailPtr(addrBits - 1, blockBits) + 1.U
      }.otherwise {
        addr := tailPtr(addrBits - 1, blockBits)
      }
    }

  // Todo: potential optimization here? including mask PopCount in StreamResponse removes adder chain
  // Todo: at cost of StreamResponse having extra field, but need one adder for padding option here?

  // Calculate raw tail ptr
  rawNxtTailPtr := PopCount(writeMask) + tailPtr

  // Connect read and write wire
  seqMemBlock
    .zip(seqWriteAddr.zip(seqWriteData.zip(seqWriteEnable)))
    .zip(seqReadAddr.zip(seqReadData.zip(seqReadEnable)))
    .foreach { case ((mem, (writeAddr, (writeData, writeEnable))), (readAddr, (readData, readEnable))) =>
      // Connect DualPort1w1rSyncReadRAM IO
      mem.writeAddr := writeAddr
      mem.writeData := writeData.asUInt()
      mem.writeEnable := writeEnable
      readData := mem.readData.asTypeOf(genType)
      mem.readAddr := readAddr
      mem.readEnable := readEnable
    }

  // Connect Dequeue to Read Addr, Enable
  seqReadAddr.zip(seqReadEnable).zipWithIndex.foreach { case ((addr, enable), idx) =>
    // Generate dequeue to read one hot selection
    require(seqDeqBlockSel.length == vecOutputReady.length)
    val sel1H: Seq[Bool] = seqDeqBlockSel.zip(vecOutputReady).map { case (sel, ready) => ready && sel === idx.U }
    // Connect to address (depth selection)
    require(sel1H.length == seqDeqDepthSel.length)
    if (depthBits > 0) addr := Mux1H(sel1H, seqDeqDepthSel)
    // Connect to enable, read memory no matter whether or not can be dequeued
    enable := VecInit(sel1H).asUInt().orR()
  }

  // Connect Read Data to Dequeue
  require(vecOutput.length == seqDeqBlockSel.length)
  require(vecOutput.length == vecOutputReady.length)
  vecOutput.zip(vecOutputReady).zip(seqDeqBlockSel.zipWithIndex).foreach { case ((out, ready), (blockSel, outIdx)) =>
    // Connect bits, one cycle delay since it is SyncReadMem
    val prevBlockSel: UInt = RegNext(blockSel)
    // Calculate whether this output port is covered by padding
    val padIdx: Int = outIdx / numUnitPerPad
    val padded: Bool = padIdx.U < numPadLeft.getOrElse(0.U)
    // Connect bits, if output port does not fall in padded range (either pad off or pad zero), bits connect to zero
    out.bits := Mux(pad && !padded, 0.U, seqReadData(prevBlockSel))
    // Connect valid, delay one cycle since it is sync read mem
    out.valid := Mux(
      padOff.getOrElse(false.B),
      // Padding off
      padded && deqFireByPadWire,
      // No padding off
      Mux(
        padZero.getOrElse(false.B),
        // Padding zero
        deqFireByPadWire,
        // No Padding at all
        deqFireReg
      )
    ) && ready
  }

  /* ------------------------- Finite State Machine         ------------------------- */

  // Update Tail Pointer Register when input valid exist anc can do enqueue
  tailPtrReg match {
    case Some(value) => when(enqFireWire) { value := nextTailPtr }
    case None        =>
  }

  // Will be triggered by next deq firing
  deqFireReg := deqFireWire

  // Update Head Pointer Register when output ready exist and can do dequeue
  headPtrReg match {
    case Some(value) =>
      when(moveHead) {
        // dequeue side (head) is fired, change head to next one
        value := nextHeadPtr
      }
    case None =>
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Output statistic
  count := numElem
  left := numSpace

  // Input Ready
  vecInput.foreach(in => in.ready := canEnqBySpace)

  // Empty / Full
  full := false.B // fullWire
  empty := RegNext(emptyWire)

  // Enqueue / Dequeue Fired
  enqFire := enqFireWire
  deqFire := deqFireReg || deqFireByPadWire // Fire regular or Fire by pad
}