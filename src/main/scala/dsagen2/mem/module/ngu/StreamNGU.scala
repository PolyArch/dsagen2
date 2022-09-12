package dsagen2.mem.module.ngu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.{StreamRequest, StreamResponse, StreamState}
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.sync.module.MuxXBar
import dsagen2.top.diplomacy.DSANodeType.GenerateEngine
import dsagen2.util.QueueFPGA
import dsagen2.util.UIntUtil.groupBitsAs

/** Stream Numerical Sequence Generation Unit
  *
  * Input:
  *  - Stream Request (for address + mask + meta info)
  *    Output:
  *  - Stream Response (numerical data sequence)
  *
  * Internal:
  *  - Queue to buffer stream request, since stream request may contain up to #bandwidth address sequence
  *    and the worst case is that address data type is minimum but generated stream is maximum data type
  *    which means that multiple cycles are needed to convert one stream request to stream response
  */
class StreamNGU(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit val p: Parameters)
    extends MultiIOModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  // The bit width of generated vector of value
  val bitwidth: Int = memNode.bandBits

  /* ------------------------- Derived Parameters           ------------------------- */

  // Maximum number of address sequence from stream request. We assume the minimum memory data type (byte) is supported
  def maxNumAddr: Int = memNode.bandwidth

  // Minimum number of numerical sequence to be sent to stream response
  def minNumSeq: Int = memNode.bandBits / memNode.maxConstDataBits

  // Number of bits for the local offsets
  def offsetBits: Int = log2Ceil(memNode.bandwidth)

  // How many unit per max const data type bits
  def maxNumUnitPerConst: Int = memNode.maxConstDataBits / memNode.memUnitBits

  // Request Queue Depth for Generate Engine
  def genQueueDepth: Int = 4 // memNode.numPendingRequest

  /* ------------------------- Diplomatic Node              ------------------------- */
  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // Node Type Check
  require(
    memNode.nodeType == GenerateEngine,
    s"Only generate engine need this module, but node type is ${memNode.nodeType}"
  )

  // Max constant data type bits
  require(
    memNode.bandBits >= memNode.maxConstDataBits,
    s"The memory bit width ${memNode.bandBits} need to be wider than the " +
      s"max const data type bits ${memNode.maxConstDataBits}"
  )

  /* ------------------------- Input / Output               ------------------------- */

  // Stream Request from AGU, the length should be just one
  val strRequest: StreamRequest = IO(Input(new StreamRequest(memNode, ivpsParam, ovpsParam)))

  // Stream Response Ready from Input vector port bus
  val strAccepted: Bool = IO(Input(Bool()))

  // Stream Response that converts address + mask + constDataType to vector of value
  val strResponse: StreamResponse = IO(Output(new StreamResponse(memNode, ivpsParam, ovpsParam)))

  // Pause: stream request to stream response convertion pause
  val convertPause: Bool = IO(Output(Bool()))

  /* ------------------------- Registers                    ------------------------- */
  /* ------------------------- Modules                      ------------------------- */

  // Queue to buffer stream request, a shallow queue is enough
  val requestQueue = Module(
    new QueueFPGA[StreamRequest](new StreamRequest(memNode, ivpsParam, ovpsParam), genQueueDepth, p(FPGAOverlay), dontTouch = false)
  )

  // Crossbar that routes the offset to the correct place
  val offsetXBar: MuxXBar[UInt] = Module(new MuxXBar[UInt](UInt(offsetBits.W), memNode.bandwidth, memNode.bandwidth))

  // XBar Router to control the offset cross bar
  val xbarCtrl: NGUXBarRouter = Module(
    new NGUXBarRouter(memNode.bandwidth, memNode.numMemDataTypeExp, memNode.numGenDataType)
  )

  /* ------------------------- Wires                        ------------------------- */

  // Start point whose width is the same as max const data type bits
  val startPoint: UInt = WireInit(0.U(memNode.maxConstDataBits.W))
  // Offset info is embedded in mask, to set lower bits to 0
  val startPoint_hi: UInt = (requestQueue.io.deq.bits.addr
    .getOrElse(0.U(memNode.maxConstDataBits.W)) >> log2Ceil(memNode.bandwidth)).asUInt()
  val startPoint_lo: UInt = 0.U(log2Ceil(memNode.bandwidth).W)
  startPoint := Cat(startPoint_hi, startPoint_lo)

  // Group start point as memory unit bit width
  val startPointGroup: Vec[UInt] = VecInit(groupBitsAs(startPoint, memNode.memUnitBits))

  // Mask from the dequeue port of request queue
  val mask: Seq[Bool] = requestQueue.io.deq.bits.mask.getOrElse(0.U(memNode.bandwidth.W)).asBools()

  // The data type exponential of generate stream
  val constExp: UInt = requestQueue.io.deq.bits.meta.constDataTypeExp.getOrElse(0.U)

  // A sequence of local offset to the address from stream request, has not been routed to the correct place
  val offsetsInorder: Seq[UInt] = Seq.fill(memNode.bandwidth)(WireInit(0.U(offsetBits.W)))
  offsetsInorder.zipWithIndex.zip(mask).foreach { case ((offset, idx), maskValid) =>
    offset := Mux(requestQueue.io.deq.valid && maskValid, idx.U, 0.U)
  }

  // Mapping from data truck location to the trunk of start point, with consideration of exp
  val unitIdx2valueTrunk: Seq[UInt] = for (unitIdx <- 0 until maxNumUnitPerConst) yield {
    // If unit index is zero, then it is definitely the lowest trunk
    if (unitIdx == 0) startPointGroup.head
    else {
      // Calculate the largest exp that will affect unit at this position
      val largestExp: Int = largestExpPerUnitIdxInMaxConstData(unitIdx)
      // If it is not the lowest trunk, we need to consider cases that one const data cannot cover this unit index
      // valueTrunk = exp2valueTrunk[exp]
      val exp2valueTrunk: Seq[(UInt, UInt)] =
        for (exp <- 0 to largestExp) yield {
          val valueTrunk: UInt =
            if (exp == 0) startPointGroup.head
            else {
              // Calculate the unit index with consideration of Exp
              val unitIdxExp: Int = unitIdx % (1 << exp)
              // Access the value trunk by unit index of exponential
              startPointGroup(unitIdxExp)
            }
          exp.U -> valueTrunk
        }
      // Zip with exponential and MuxLookup
      MuxLookup(constExp, startPointGroup(unitIdx), exp2valueTrunk)
    }
  }

  // Sequence of value trunk with total length = bandwidth
  val valueTrunks: Seq[UInt] = for (unitIdx <- 0 until memNode.bandwidth) yield {
    val localUnitIdx: Int = unitIdx % maxNumUnitPerConst
    unitIdx2valueTrunk(localUnitIdx)
  }
  valueTrunks.zipWithIndex.foreach { case (trunk, unitIdx) =>
    trunk.suggestName(s"valueTrunk_$unitIdx")
    require(
      trunk.getWidth == memNode.memUnitBits,
      s"For each data trunk , it should be ${memNode.memUnitBits}-bit, but for $unitIdx-th data trunk, it is " +
        s"${trunk.getWidth}-bit"
    )
  }

  // Routed offsets from XBar
  val offsetsRouted: Seq[UInt] = offsetXBar.vecOutput.map(_.bits)

  // Wire that hold the carrier from previous adder, cut off by exponential
  val valueInCarries: Seq[UInt] = Seq.fill(memNode.bandwidth)(WireInit(0.U(1.W)))

  // Wire that add the routed offset and local data trunk together and produce the carrier
  require(
    offsetsRouted.length == valueTrunks.length,
    s"The length of routed offsets ${offsetsRouted.length} != length of value trunk ${valueTrunks.length}, " +
      s"which should be equal to memory bandwidth ${memNode.bandwidth}"
  )
  val valueSumsCarries: Seq[(UInt, UInt)] =
    offsetsRouted.zip(valueTrunks).zip(valueInCarries).map { case ((off, trunk), c) =>
      require(off.getWidth == offsetBits, s"Offset should be $offsetBits, but it is ${off.getWidth}")
      require(
        trunk.getWidth == memNode.memUnitBits,
        s"Data trunk should be ${memNode.memUnitBits}, " +
          s"but it is ${trunk.getWidth}"
      )
      val sumWithCarry: UInt = c +& off +& trunk
      require(sumWithCarry.getWidth == off.getWidth.max(trunk.getWidth) + 1)
      val sum:   UInt = sumWithCarry(sumWithCarry.getWidth - 2, 0)
      val carry: UInt = sumWithCarry(sumWithCarry.getWidth - 1)
      (sum, carry)
    }

  // Wire that extract sums from above
  val valueSums: Seq[UInt] = valueSumsCarries.map(_._1)

  // Wire that extract carries from above
  val valueOutCarries: Seq[UInt] = valueSumsCarries.map(_._2)
  require(valueOutCarries.forall(_.getWidth == 1))

  /* ------------------------- Combination Logic            ------------------------- */

  // Enqueue to the request queue, backpressure (enq.ready is handled by convertPause)
  requestQueue.io.enq.valid := strRequest.valid
  requestQueue.io.enq.bits := strRequest

  // Connect Input Side of XBar (Offset to XBar)
  offsetXBar.vecInput
    .zip(requestQueue.io.deq.bits.mask.getOrElse(0.U(memNode.bandwidth.W)).asBools())
    .zipWithIndex
    .foreach { case ((vInPort, valid), vIx) =>
      vInPort.valid := valid
      require(vInPort.bits.getWidth == log2Ceil(memNode.bandwidth))
      vInPort.bits := vIx.U
    }

  // Connect the XBar Output
  // Since ready to requestQueue is controlled by XBar controlled, it is ignored here
  offsetXBar.vecOutput.foreach { vOutPort => vOutPort.ready := DontCare }

  // Connect Input Carry from previous Output Carry, turn off by exponential
  valueInCarries.zipWithIndex.foreach { case (inputCarry, unitIdx) =>
    // Get previous carry output
    val prevCarry: UInt = if (unitIdx == 0) 0.U(1.W) else valueOutCarries(unitIdx - 1)
    // Width check of previous carry
    require(prevCarry.getWidth == 1, s"Carry should be one bit, but it is ${prevCarry.getWidth}")
    // Whether current place need carry from previous location, taking exponential into account
    val needCarry: Bool = {
      if (unitIdx == 0) {
        false.B
      } else {
        // Calculate the local unit index in largest const data type
        val localIndex: Int = unitIdx % maxNumUnitPerConst
        // Calculate the largest exponential that will affect current position index
        val largestExp: Int = largestExpPerUnitIdxInMaxConstData(unitIdx)
        // Exponential to whether or not need carry
        val exp2inputCarry: Seq[(UInt, Bool)] = for (exp <- 0 to largestExp) yield {
          // Exponential == 0 means the finest unit, which does not need carry input
          exp.U -> {
            if (exp == 0) false.B
            // As long as the current location is not the lowest, it needs carry
            else (localIndex % (1 << exp) != 0).B
          }
        }
        // Map exp to the need of carry
        MuxLookup(constExp, true.B, exp2inputCarry)
      }
    }
    // Connect input carry from previous carry
    inputCarry := needCarry && prevCarry.asBool()
  }

  // Connect from request queue to crossbar routing controller
  xbarCtrl.inputValid := requestQueue.io.deq.valid
  xbarCtrl.inAddrMask := requestQueue.io.deq.bits.mask.get
  requestQueue.io.deq.ready := xbarCtrl.inputReady
  xbarCtrl.mExp match {
    case Some(mExp) => mExp := requestQueue.io.deq.bits.meta.memDataTypeExp.getOrElse(0.U)
    case None       =>
  }
  xbarCtrl.cExp match {
    case Some(cExp) => cExp := requestQueue.io.deq.bits.meta.constDataTypeExp.getOrElse(0.U)
    case None       =>
  }
  xbarCtrl.outputReady := strAccepted
  require(
    offsetXBar.sels.length == xbarCtrl.offsetRoutes.length,
    s"The number of offset xbar selection port ${offsetXBar.sels.length} should be equal to " +
      s"xbar controller output routing port ${xbarCtrl.offsetRoutes.length}"
  )
  offsetXBar.sels.zip(xbarCtrl.offsetRoutes).foreach { case (xbarSel, route) =>
    require(
      xbarSel.getWidth == route.getWidth,
      s"Routing selection bit should be equal : " +
        s"${xbarSel.getWidth} != ${route.getWidth}"
    )
    xbarSel := route
  }

  /* ------------------------- Finite State Machine         ------------------------- */
  /* ------------------------- Output Connection            ------------------------- */

  // Stream Table selection should be paused if the there is only two elements left in stream request buffer
  require(genQueueDepth > 3, s"Generate Queue cannot be shallower then 2, but it is $genQueueDepth")
  convertPause := requestQueue.io.count > (genQueueDepth - 2).U

  // Output to the stream response read data
  require(strResponse.readData.isDefined, s"Generate Engine should contains generated stream as read data")
  require(
    strResponse.readData.get.getWidth == valueSums.map(_.getWidth).sum,
    s"The read response data as defined in stream response width ${strResponse.readData.get.getWidth}, is not equal to" +
      s"the total sum results width ${valueSums.map(_.getWidth).sum}, which should be equal to memory band bits" +
      s" ${memNode.bandBits}"
  )
  strResponse.readData.get := VecInit(valueSums).asUInt()

  // Connect the mask from XBar controller
  strResponse.mask match {
    case Some(valueMask) =>
      require(
        valueMask.getWidth == xbarCtrl.valueMask.getWidth,
        s"ValueMask from Crossbar controller width ${xbarCtrl.valueMask.getWidth} should be equal to the one in stream " +
          s"response ${valueMask.getWidth}"
      )
      valueMask := xbarCtrl.outputValueMask
    case None =>
  }

  // Connect to Stream Response Valid / Meta / State
  strResponse.valid := requestQueue.io.deq.valid
  strResponse.meta := requestQueue.io.deq.bits.meta
  strResponse.state match {
    case Some(state) => state := requestQueue.io.deq.bits.state.getOrElse(0.U.asTypeOf(new StreamState))
    case None        =>
  }

  /* ------------------------- Hardware Sanity Check        ------------------------- */
  /* ------------------------- Post Generation Sanity Check ------------------------- */

  // Input fields existence check
  require(strRequest.writeData.isEmpty, s"Generate engine cannot take write data from OVP")
  require(strRequest.meta.mStatus.isEmpty, s"Generate should not contains MStatus, it is DMA only")

  /* ------------------------- Utility                      ------------------------- */

  // Calculate the largest exponential that will affect the given unit index in max const data type
  private def largestExpPerUnitIdxInMaxConstData(unitIdx: Int): Int = {
    require(
      unitIdx < maxNumUnitPerConst,
      s"This function should only be called for unit index that is in one max const data"
    )
    log2Floor(unitIdx) + 1
  }
}
