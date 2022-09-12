package dsagen2.sync.module

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import dsagen2.mem.bundle.StreamState
import dsagen2.misc.module.Mask2Routing.{inputMask2outRouting, outputMask2outRouting}
import dsagen2.sync.impl.VectorPortImpl
import dsagen2.top.config.enumeration.VPImplMode._
import dsagen2.util.AppUtil.bitvecSynchronized
import dsagen2.util.BooleanUtil.boolean2int

/** Multiple Input/Output, First In First Out Queue, the input/output order is maintained
  *
  * It is LowerMultiIOQueue + Optional Input Aggregator and Optional Output Distributor
  *
  * The input aggregator and output distributor is actually a crossbar
  *
  * @param numInput       Number of Input Port
  * @param inputBits      Data Bits for each input port
  * @param numOutput      Number of output port
  * @param outputBits     data bits of output port
  * @param depthByte      FIFO Depth in unit of byte
  * @param inputXBarMode  The input XBar Implementation to fit different type of vector port
  * @param outputXBarMode The output XBar Implementation to fit different type of vector port
  * @param outerVPImpl The outer implementation of vector port
  */
class AggDisMultiIOQueue(
  val numInput:       Int,
  val inputBits:      Int,
  val numOutput:      Int,
  val outputBits:     Int,
  val depthByte:      Int,
  val inputXBarMode:  VPImpl = LimitXBarVP,
  val outputXBarMode: VPImpl = LimitXBarVP,
  val outerVPImpl:    VectorPortImpl)
                        (implicit p: Parameters)

    extends MultiIOModule {
  /* -------------------------      Extract Parameters      ------------------------- */

  val isIVP:      Boolean = outerVPImpl.isIVP
  val statedIVP:  Boolean = outerVPImpl.statedIVP
  val isOVP:      Boolean = !isIVP
  val statedOVP:  Boolean = outerVPImpl.statedOVP
  val supportPad: Boolean = outerVPImpl.supportPad
  val padWidth:   Int = outerVPImpl.padWidth

  /* -------------------------     Derived Parameters       ------------------------- */

  // Final Depth which is power of 2, ceiling to the nearest power of 2 number as depth if give depth is not power of 2
  def finalDepth: Int = 1 << log2Ceil(depthByte)

  // Minimum data bits, unitBits of this queue
  def unitBits: Int = inputBits.min(outputBits)

  // Minimum depth of this queue in unit of minDataBits-bit
  def minDepth: Int = ((numInput * inputBits).max(numOutput * outputBits)) / unitBits

  // Depth in bits
  def depthBits: Int = finalDepth * unitBits

  // Output to Minimum Ratio
  def output2min: Int = outputBits / unitBits

  // Input to Minimum Bits Ratio
  def input2min: Int = inputBits / unitBits

  // Depth in output bits granularity
  def depthOutput: Int = depthBits / outputBits

  // Name this module
  suggestName(
    s"adQ_i${numInput}x${inputBits}b_o${numOutput}x${outputBits}b_" +
      s"d${depthByte}_iM${inputXBarMode}_oM$outputXBarMode"
  )

  /* -------------------------      Parameters Sanity Check ------------------------- */

  // depth needs to be larger than minimum depth
  require(
    finalDepth >= minDepth,
    s"This Multi IO FIFO require at least $minDepth (unit is $unitBits-bit) as depth, " +
      s"but the given is $finalDepth. " +
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
    isPow2(finalDepth),
    s"Finest granularity of queue is $unitBits-bit, which results in" +
      s"depth of minimum bits = $finalDepth, it needs to be power of 2"
  )

  // Input Bits and Output Bits should be multiple relationship
  require(
    inputBits % outputBits == 0 || outputBits % inputBits == 0,
    s"Input Bits = $inputBits, Output Bits = $outputBits, they are not in multiple relationship"
  )

  // Stated Vector Port must make sence
  require(!(statedIVP && statedOVP), "Vector port cannot be both stated IVP and stated OVP")
  if (statedIVP) require(outputBits >= 6 && !isOVP, s"Output bits cannot carry 6-bit stream state, isOVP = $isOVP")
  if (statedOVP) require(inputBits >= 6 && isOVP, s"Input bits cannot carry 6-bit stream state, isOVP = $isOVP")

  // Padding Width check
  if (!supportPad) require(padWidth == -1, s"Padding width should be -1 if no padding supported")
  // TODO: does padding width has to be power of 2?
  else require(requirement = true /*isPow2(padWidth)*/, s"Padding width should be power of 2, but it is $padWidth")

  /* -------------------------         Input / Output       ------------------------- */

  // O(ready), Vector Input
  val vecInput: Vec[DecoupledIO[UInt]] = IO(Flipped(Vec(statedOVP + numInput, DecoupledIO(UInt(inputBits.W)))))

  // Input Synchronized
  val inputSyncs: Vec[Bool] = IO(Input(Vec(statedOVP + numInput, Bool())))

  // O, Vector Output
  val vecOutput: Vec[DecoupledIO[UInt]] = IO(Vec(statedIVP + numOutput, DecoupledIO(UInt(outputBits.W))))

  // Output Synchronized
  val outputSyncs: Vec[Bool] = IO(Input(Vec(statedIVP + numOutput, Bool())))

  // O, Number of valid unit in queue
  val count: UInt = IO(Output(UInt(log2Ceil(finalDepth + 1).W)))
  val left: UInt = IO(Output(UInt(log2Ceil(finalDepth + 1).W)))

  // O, Whether this Queue is full/empty
  val full:  Bool = IO(Output(Bool()))
  val empty: Bool = IO(Output(Bool()))

  // I, repeat signal, if repeat is high, then head moving and invalidate will not happen
  val repeatDequeue: Bool = IO(Input(Bool()))

  // Dequeue and Enqueue Fire
  val deqFire: Bool = IO(Output(Bool()))
  val enqFire: Bool = IO(Output(Bool()))

  // Stated Port to/from compute side
  val statedPort: Option[DecoupledIO[StreamState]] = {
    // For IVP, this is inward port for output xbar routing
    if (statedIVP) Some(IO(Flipped(DecoupledIO(new StreamState))))
    // For OVP, this is outward port that pass input xbar routing
    else if (statedOVP) Some(IO(DecoupledIO(new StreamState)))
    else None
  }

  // Number of active compute side port
  val numActCompPort: Option[UInt] = {
    // For IVP, it is used for calculating the number of needed unit for (padding)
    if (statedIVP) { Some(IO(Output(UInt(log2Ceil(numOutput + 1).W)))) }
    // FOr OVP, this is the
    else if (isOVP) { Some(IO(Output(UInt(log2Ceil(numInput + 1).W)))) }
    else None
  }

  // Number of needed compute side port for OVP to enqueue the right amount of data
  val numNeedCompPort: Option[UInt] = if (isOVP) Some(IO(Input(UInt(log2Ceil(numInput + 1).W)))) else None

  // Padding Control
  val padZero:    Option[Bool] = if (supportPad) Some(IO(Input(Bool()))) else None
  val padOff:     Option[Bool] = if (supportPad) Some(IO(Input(Bool()))) else None
  val numPadLeft: Option[UInt] = if (supportPad) Some(IO(Input(UInt(log2Ceil(padWidth).W)))) else None

  /* -------------------------     Registers and Modules    ------------------------- */

  // Continuous lowest Only Multi-IO Queue
  val didoQueue: LowerMultiIOQueue = Module(
    new LowerMultiIOQueue(
      numInput,
      inputBits,
      numOutput,
      outputBits,
      finalDepth,
      supportPad,
      padWidth
    )
  )

  // Downward crossbar to aggregate the input side connection
  val inputXBar: MuxXBar[UInt] = Module(
    new MuxXBar[UInt](UInt(inputBits.W), statedOVP + numInput, statedOVP + numInput, inputXBarMode == NonXBarVP)
  )

  // Downward crossbar to distribute the output side connection
  val outputXBar: MuxXBar[UInt] = Module(
    new MuxXBar[UInt](UInt(outputBits.W), statedIVP + numOutput, statedIVP + numOutput, outputXBarMode == NonXBarVP)
  )

  /* ------------------------- Wires                        ------------------------- */

  // Use the input valid as routing mask for the input side
  // Aggregate the input to the lowest part of inner queue
  // This is self-routing, which should do the work of mask aggregator
  val inRoute: Seq[UInt] = {
    // Input Routing Info is from outside
    if (inputXBarMode == FullXBarVP) {
      // Full crossbar, expose the routing to outside world
      IO(Input(Vec(statedOVP + numInput, UInt(log2Ceil(statedOVP + numInput + 1).W))))
    } else if (inputXBarMode == LimitXBarVP) {
      // Input Routing is from it self routed, as aggregation
      inputMask2outRouting(vecInput.map(_.valid))
    } else {
      // No routing is needed if non crossbar design is used
      require(inputXBarMode == NonXBarVP)
      Seq.fill(vecInput.length)(0.U)
    }
  }

  // Use the output ready as routing mask for the output side
  // Distribute the lowest part of inner queue output to each output port
  // This is self-routing, which should do the work of mask distributor
  val outRoute: Seq[UInt] = {
    if (outputXBarMode == FullXBarVP) {
      // Full crossbar, expose the routing to outside world
      IO(Input(Vec(statedIVP + numOutput, UInt(log2Ceil(statedIVP + numOutput + 1).W))))
    } else if (outputXBarMode == LimitXBarVP) {
      // Self generated routing as distribution
      outputMask2outRouting(vecOutput.map(_.ready))
    } else {
      // No routing is needed if non-crossbar design is used
      require(outputXBarMode == NonXBarVP)
      Seq.fill(vecOutput.length)(0.U)
    }
  }

  // Route input synchronization signal to correct place
  // Zero means route from nowhere, do connect to false, 1 means the first port
  val lowerInputSync: Seq[Bool] =
    if (inputXBarMode != NonXBarVP) inRoute.map(route => Mux(route === 0.U, false.B, inputSyncs(route - 1.U)))
    else inputSyncs

  // Calculate the output backward one hot routing,  starting from 1 since 0 selects ground (nowhere)
  val outSyncOneHots: Seq[Seq[Bool]] = (1 to statedIVP + numOutput).map { routeVal =>
    outRoute.map(x => x === routeVal.U)
  }
  // Route back output synchronization signal to correct place
  val lowerOutputSync: Seq[Bool] =
    if (outputXBarMode != NonXBarVP) outSyncOneHots.map { onehot => Mux1H(onehot, outputSyncs) }
    else outputSyncs

  // Whether input / output sync is required
  val inputSynced:  Bool = inputSyncs.asUInt().orR()
  val outputSynced: Bool = outputSyncs.asUInt().orR()

  // OVP Synchronization bits
  val ovpCompSyncs: Option[Seq[Bool]] = if (isOVP) Some(lowerInputSync.drop(statedOVP)) else None
  // OVP Synchronized need bits
  val ovpCompNeeds: Option[Seq[Bool]] =
    if (isOVP) Some(lowerInputSync.drop(statedOVP).indices.map(idx => idx.U < numNeedCompPort.get))
    else None
  // OVP Synchronized bit
  val ovpCompSynced: Option[Bool] = ovpCompSyncs match {
    case Some(syncs) =>
      // Get the lower valid bits
      val lowerValids: Seq[Bool] = inputXBar.vecOutput.drop(statedOVP).map(_.valid)
      // Synchronized with valid bits, OR it
      Some(VecInit(bitvecSynchronized(lowerValids, Some(syncs))).asUInt().orR())
    case None => None
  }
  // IVP Synchronized bits
  val ivpCompSyncs: Option[Seq[Bool]] =
    if (isIVP) Some(bitvecSynchronized(vecOutput.map(_.ready), Some(outputSyncs))) else None
  val ivpCompSynced: Option[Bool] = ivpCompSyncs match {
    case Some(syncs) => Some(VecInit(syncs).asUInt().orR())
    case None        => None
  }

  /* -------------------------     Combination Logics     ------------------------- */

  // All ready signal from the input side of inner queue
  val allReady: Bool = VecInit(didoQueue.vecInput.map(_.ready)).asUInt().andR()

  // All valid signal from the output side of inner queue
  val allValid: Bool = VecInit(didoQueue.vecOutput.map(_.valid)).asUInt().andR()

  // Connect the input side XBar with inner queue
  require(statedOVP + didoQueue.vecInput.length == inputXBar.vecOutput.length)
  require(didoQueue.vecInput.head.bits.getWidth == inputXBar.vecOutput.head.bits.getWidth)
  didoQueue.vecInput.zip(inputXBar.vecOutput.drop(statedOVP)).zipWithIndex.foreach {
    case ((vIqueue, vOxbar), compDataIdx) =>
      require(vIqueue.getWidth == vOxbar.getWidth)
      if (isOVP) {
        // Connect bits
        vIqueue.bits := vOxbar.bits
        // For OVP, only enqueue the right amount of element
        vIqueue.valid := vOxbar.valid && ovpCompNeeds.get.apply(compDataIdx) && ovpCompSynced.get
        vOxbar.ready := vIqueue.ready && ovpCompSynced.get
      } else vIqueue <> vOxbar
  }
  if (statedOVP) {
    statedPort.get.bits := inputXBar.vecOutput.head.bits.asTypeOf(new StreamState)
    statedPort.get.valid := inputXBar.vecOutput.head.valid && didoQueue.enqFire
    inputXBar.vecOutput.head.ready := statedPort.get.ready
  }

  // Connect the output side: XBar with inner queue
  require(statedIVP + didoQueue.vecOutput.length == outputXBar.vecInput.length)
  require(didoQueue.vecOutput.head.bits.getWidth == outputXBar.vecInput.head.bits.getWidth)
  outputXBar.vecInput.drop(statedIVP).zip(didoQueue.vecOutput).foreach { case (vIxbar, vOqueue) =>
    require(vIxbar.getWidth == vOqueue.getWidth)
    vIxbar <> vOqueue
  }
  if (statedIVP) {
    // Downward connection
    outputXBar.vecInput.head.bits := statedPort.get.bits.asUInt()
    outputXBar.vecInput.head.valid := statedPort.get.valid && ivpCompSynced.get
    // Upward ready, calculate whether there are data flow using state
    val hasSink: Bool = VecInit(outSyncOneHots.head).asUInt().orR()
    // If no data flow is using state queue, always ready (controlled by other ports deqFire)
    statedPort.get.ready := Mux(hasSink, outputXBar.vecInput.head.ready && lowerOutputSync.head, true.B) &&
      ivpCompSynced.get
  }

  // Connect the aggregated synchronization signal to the inner queue
  require(statedOVP + didoQueue.inputSyncs.length == lowerInputSync.length)
  didoQueue.inputSyncs.zip(lowerInputSync.drop(statedOVP)).zipWithIndex.foreach {
    case ((queueSync, lowerSync), compDataIdx) =>
      queueSync := lowerSync && { if (isOVP) ovpCompNeeds.get.apply(compDataIdx) && ovpCompSynced.get else true.B }
  }

  // Connect the aggregated synchronization signal to the inner queue
  require(
    statedIVP + didoQueue.outputSyncs.length == lowerOutputSync.length,
    s"State IVP = $statedIVP, didoQueue.outputSyncs.length = ${didoQueue.outputSyncs.length}, " +
      s"lowerOutputSync.length = ${lowerOutputSync.length}"
  )
  didoQueue.outputSyncs.zip(lowerOutputSync.drop(statedIVP)).foreach { case (queueSync, aggSync) =>
    queueSync := aggSync
  }

  // Connect the routing to the input side XBar
  require(inputXBar.sels.length == inRoute.length)
  inputXBar.sels.zip(inRoute).foreach { case (sel, routing) =>
    sel := routing
  }

  // Connect the routing to the output side XBar
  require(outputXBar.sels.length == outRoute.length)
  outputXBar.sels.zip(outRoute).foreach { case (sel, routing) =>
    sel := routing
  }

  // Connect padding wire
  if (supportPad) {
    didoQueue.padZero.get := padZero.get
    didoQueue.padOff.get := padOff.get
    didoQueue.numPadLeft.get := numPadLeft.get
  }

  /* -------------------------       Output Connection      ------------------------- */

  // Connect the input side with the input side XBar
  require(vecInput.length == inputXBar.vecInput.length)
  require(vecInput.head.bits.getWidth == inputXBar.vecInput.head.bits.getWidth)
  vecInput.zip(inputXBar.vecInput).foreach { case (vI, vIxbar) =>
    require(vI.getWidth == vIxbar.getWidth)
    // Connect data bits and valid
    vIxbar.bits := vI.bits
    vIxbar.valid := vI.valid
    // Connect ready: Either all input ports are ready or fired and corresponding port are ready
    // All ready can only be passed if no input sync is required
    vI.ready := Mux(!inputSynced, allReady, false.B) || { if (isOVP) vIxbar.ready else false.B }
  }

  // Connect the output side with the output side XBar
  require(vecOutput.length == outputXBar.vecOutput.length)
  require(vecOutput.head.bits.getWidth == outputXBar.vecOutput.head.bits.getWidth)
  vecOutput.zip(outputXBar.vecOutput).foreach { case (vO, vOxbar) =>
    require(vO.getWidth == vOxbar.getWidth)
    // Connect data bits
    vO.bits := vOxbar.bits
    // Connect valid: either all valid or dequeue fired and corresponding port is valid
    // All valid can only be passed if no output sync is required
    vO.valid := Mux(!outputSynced, allValid, false.B) || didoQueue.deqFire && vOxbar.valid
    // Connect ready
    vOxbar.ready := vO.ready && ivpCompSynced.getOrElse(true.B)
  }

  // Just forward the inner enqueue and dequeue fired
  enqFire := didoQueue.enqFire
  deqFire := didoQueue.deqFire

  // Connect the number of existing unit in queue by forwarding the signal of inner queue
  count := didoQueue.count
  left := didoQueue.left

  // Connect the fullness
  full := didoQueue.full
  empty := didoQueue.empty

  // Connect the repeat queue signal
  didoQueue.repeatDequeue := repeatDequeue

  // Connect the number of active compute port
  if (statedIVP) {
    val compSyncs:   Seq[Bool] = lowerOutputSync.drop(1)
    val allCompSync: Bool = VecInit(compSyncs).asUInt().andR()
    numActCompPort.get := Mux(
      allCompSync,
      compSyncs.length.U, // All compute port enabled
      PriorityEncoder(~VecInit(lowerOutputSync.drop(1)).asUInt())
    )
  } else if (isOVP) {
    require(lowerInputSync.length == inputXBar.vecOutput.length)
    val compActs: Seq[Bool] =
      lowerInputSync.drop(statedOVP).zip(inputXBar.vecOutput.map(_.valid).drop(statedOVP)).map { case (sync, valid) =>
        sync && valid
      }
    val allCompAct: Bool = VecInit(compActs).asUInt().andR()
    // Get the amount of sync compute port with valid data, since it has been aggregated to lower position
    // we can use cheap priority encoder to get the first position that is not sync or not valid
    // which is equal to the amount to sync/valid compute port
    numActCompPort.get := Mux(allCompAct, compActs.length.U, PriorityEncoder(~VecInit(compActs).asUInt()))
  }
}
