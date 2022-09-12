package dsagen2.sync.impl

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import dsagen2.comp.bundle.CompDirBundle
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.reconf.CompReconfEdge
import dsagen2.comp.diplomacy.CompDirEdgeParameters
import dsagen2.mem.bundle.{MemReadBundle, StreamState}
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.Mem2IVPParameter
import dsagen2.sync.bundle.{IVPMemBus, IVPSetPort, VPRouteCfgBits, VectorPortStatus}
import dsagen2.sync.config.IVPNodeParameters
import dsagen2.sync.config.SyncKeys.IVPNode
import dsagen2.sync.module.{AggDisMultiIOQueue, StreamStateQueue}
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.config.enumeration.VPImplMode._
import dsagen2.util.BooleanUtil.boolean2int
import dsagen2.util.StreamUtil.vecDataConnect
import freechips.rocketchip.tile.XLen

import scala.collection.mutable.ListBuffer
import scala.math._

/** The hardware implementation of input vector port
  *
  * @param ivpNode       Input Vector Port Node Parameter
  * @param reconfParam   Reconfigurable Parameter, Optional for VP. Non-XBar has no reconfigure port
  * @param memReadParams Memory Read Side Parameter
  * @param compParams    Compute Side Parameter
  * @param numIVP        Number of total IVP
  * @param p             CDE Parameter
  */
class InputVectorPortImp(
  val ivpNode:       IVPNodeParameters,
  val reconfParam:   Option[CompReconfEdge],
  val memReadParams: Seq[Mem2IVPParameter],
  val compParams:    Seq[CompDirEdgeParameters],
  val numIVP:        Int
)(
  implicit val p: Parameters)
    extends VectorPortImpl {

  /* ------------------------- Extract Parameters           ------------------------- */

  // Get the DSAGEN Node ID for this input vector port, should only be used for debug, not hardware specification
  val vpNodeId:  Int = ivpNode.getNodeId
  val ivpNodeId: Int = vpNodeId
  val vpNodeTpe: UInt = ivpConfType

  // Compute Side XBar implementation
  val vpImpl: VPImpl = ivpNode.vpImpl

  // Get the depth of vector port in unit of byte
  val depthByte: Int = ivpNode.depthByte

  // Print debug info for this input vector port node
  if (printDebug) println(s"${memReadParams.length} Mem(s) --> ${ivpNode.getNodeName} --> ${compParams.length} Comp(s)")

  // Suggest Name for IVP
  suggestName(s"${ivpNode.getNodeName}_m${memReadParams.length}_c${compParams.length}")

  /* ------------------------- Derived Parameters           ------------------------- */

  // Whether or not this input vector port support broadcast stream
  def supportBroadcast: Boolean = ivpNode.broadcastIVP && numIVP > 1

  // Whether or not this input vector port support stream repeat
  def supportRepeatPort: Boolean = ivpNode.repeatedIVP

  // Whether or not report stream state to the first compute node
  def supportState: Boolean = ivpNode.vpStated && memReadParams.exists(_.memNode.streamStated)

  // Check whether support linear padding
  def supportPad: Boolean = memReadParams.exists(_.memNode.LinearPadding)

  // Padding happens at the end of 1D/2D/Whole Stream, it depends on State of Stream
  if (supportPad) require(supportState, s"Padding depends on stream state")

  // Find the minimum unit bits of memory side
  def memUnitBits: Int = memReadParams.head.memNode.memUnitBits

  // Find the max memory band bit
  def maxMemBandBits: Int = memReadParams.map(_.memNode.bandBits) max

  // The Bus Width in Bus Unit
  def memWidth: Int = maxMemBandBits / memUnitBits

  // Find the minimum unit bits of compute side
  // remove the first node since it is used for stream state reporting
  def compUnitBits: Int = compParams.drop(supportState).head.compNode.compBits

  // The Total Compute Side Width in Compute Side Minimum Unit
  def compWidth: Int = compParams.length - supportState

  // Minimum depth in unit of finest granularity between memory and compute side
  // Minimum: "2 *" is because of the concept of "double buffer", 1 for compute, 1 for memory
  // Middle:  "4 *" is because of the concept of "double buffer", 2 for compute, 2 for memory
  def minDepth: Int = (2 * memWidth * memUnitBits).max(2 * compWidth * compUnitBits) / memUnitBits

  // Final Depth of Queue, unit in byte
  def finalDepth: Int = depthByte.max(minDepth)

  // The max number of vector that one request can form ==>
  // all unit from memory are active but only compute port is active
  def maxNumVecInVP: Int = finalDepth * 8 / compUnitBits

  // Set up the number of queue output (compute side vector width) routing (XBar based)
  val isIVP:    Boolean = true
  val vecWidth: Int = boolean2int(supportState) + compWidth

  def padWidth: Int = if (supportPad) compWidth else -1

  // Ratio of compute to memory
  def comp2mem: Int = compUnitBits / memUnitBits

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // Huston we have a problem
  require(compParams.nonEmpty, s"No compute node parameters found in implementation, this is not good")
  require(memReadParams.nonEmpty, s"No memory node parameters found in implementation, this is not good")

  // Sanity check: if this input vector port support stream broadcast, there must be more than two input vector port
  if (supportBroadcast)
    require(numIVP > 1, s"There are $numIVP input vector port in total, there is no need for broadcast")

  // Sanity Check, Depth cannot be too small
  // Since when people created the vector port, they do not know the nodes connected to it
  // the depth given is just a predicated depth
  // if it is not larger than the minimum depth, warning will be printed
  // Since queue related bits requires memory read bundle parameters, so it is post sanity check
  if (false)
    require(
      depthByte >= minDepth,
      s"Depth is in unit finest granularity between memory and compute side, which is $memUnitBits-bit, " +
        s"The min depth = $minDepth, but you have $depthByte"
    )

  // Sanity Check, I am super worried about the case that compute is finer than memory
  require(
    memUnitBits <= compUnitBits,
    s"The memory unit bits $memUnitBits should be " +
      s"narrower than minimum compute port $compUnitBits"
  )

  // Maximum number of vector can be formed should be positive
  require(maxNumVecInVP > 0, s"Cannot form even one vector with final depth = $finalDepth ")

  // The memory unit bits should be same for all memory engine
  require(
    memReadParams.forall(_.memNode.memUnitBits == memUnitBits),
    s"So far we only support memory engine with same granularity, should be $memUnitBits-bit addressable"
  )

  // The compute port bits should be same for all connected compute node
  require(
    compParams.forall(_.compNode.compBits == compUnitBits),
    s"So far we only support compute node with same data width connected to same IVP, should be $compUnitBits-bit wide"
  )

  // If vector port is a stated vector port, it must have at least two output connection
  if (supportState)
    require(
      compParams.length > 1,
      s"IVP ${ivpNode.getNodeId} support state, but only have " +
        s"${compParams.length} output connection, at least two are needed. One for state, one for data"
    )

  // Width of compute port should be power of 2 to memory unit
  require(isPow2(comp2mem), s"Compute port width to memory unit width should be power of 2")

  /* ------------------------- Input / Output               ------------------------- */

  // Extract the memory read bundle
  val memReadPorts: Seq[MemReadBundle] = memReadParams.map(r => IO(Flipped(new MemReadBundle(r))))

  // Extract the compute direction bundle from diplomacy node
  val compPorts: Seq[CompDirBundle] = compParams.map(r => IO(new CompDirBundle(r)))

  // Create input port for input vector port setting for repeat port and stream broadcasting
  val ivpSetPort: IVPSetPort = IO(Input(new IVPSetPort))

  // Create output port for reporting the status of input vector port
  val ivpStatus: VectorPortStatus = IO(Output(new VectorPortStatus))

  /* ------------------------- Registers                    ------------------------- */

  /* ---------- Broadcasting Stream ---------- */

  // F, If memory response for the given stream, this port also accept it
  val ivpBroadcastReg: Option[Bool] =
    if (supportBroadcast && numIVP > 1) Some(RegInit(false.B)) else None

  // F, The stream Id of broadcast stream
  val broadcastIVPortIdReg: Option[UInt] =
    if (supportBroadcast && numIVP > 1) Some(RegInit(0.U(log2Ceil(numIVP).W))) else None

  /* ---------- Padding/TagState as Vector  ---------- */

  // Register to keep track of the state of previous elements that cannot form compute vector
  val prevState: Option[StreamState] = if (supportState) Some(RegInit(0.U.asTypeOf(new StreamState))) else None

  // Register that keep track of the number of element in queue that cannot be formed as vector (leftover)
  val numLeftUnit: Option[UInt] =
    if (supportState) Some(RegInit(0.U(log2Ceil(compWidth * comp2mem + 1).W))) else None

  // Register to keep the padding type during the lifttime of stream
  val paddingReg: Option[UInt] = if (supportPad) Some(RegInit(0.U(LINEAR_PADDING_BITS.W))) else None

  /* ---------- Repeat Port as Vector  ---------- */

  // Function: repeat #repeatTime for each vector, change repeatTime after #repeatPeriod vector

  // Current Number of Repeat time for this vector, this is not in fixed point form, just unit 1
  val currRepeatTimeReg: Option[UInt] = if (supportRepeatPort) Some(RegInit(1.U(VP_REPEAT_INT_BITS.W))) else None

  // Repeat time, Fixed Point number, in granularity of number of downstream active nodes
  val repeatTimeReg: Option[UInt] =
    if (supportRepeatPort) Some(RegInit((1 << VP_REPEAT_FRAC_BITS).U(VP_REPEAT_FIXPOINT_BITS.W))) else None

  // delta to Repeat Time, Fixed Point number, apply this delta when every repeat complete
  val deltaRepeatReg: Option[UInt] =
    if (supportRepeatPort) Some(RegInit(0.U(VP_REPEAT_FIXPOINT_BITS.W))) else None

  /* ------------------------- Modules                      ------------------------- */

  // MultiIO Queue
  val queue: AggDisMultiIOQueue = Module(
    new AggDisMultiIOQueue(
      numInput = memWidth,
      inputBits = memUnitBits,
      numOutput = compWidth,
      outputBits = compUnitBits,
      depthByte = finalDepth,
      inputXBarMode = NonXBarVP,
      outputXBarMode = ivpNode.vpImpl, // IVP compute side routing is determined by configuration
      this
    )
  )

  // Queue to store the state with number of covered vector as input
  val stateQueue: Option[StreamStateQueue] =
    if (supportState) Some(Module(new StreamStateQueue(maxNumVecInVP, compWidth)))
    else None

  /* ------------------------- Wires                        ------------------------- */

  /* ---------- Common ---------- */

  // C, Aggregate the memory read port. Since it is connected to different memories, spec can be different
  val readBus: IVPMemBus = aggMemReadPorts()

  // Decode Vector Port Config Bitstream
  val configBits: Option[VPRouteCfgBits] =
    configStateReg match {
      case Some(value) => Some(value.asTypeOf(new VPRouteCfgBits(this)).suggestName("vpCfgBits"))
      case None        => None
    }

  // C, Dequeue Firing from Queue, this is truly fire, means next vector needed
  val dequeueFire: Bool = queue.deqFire

  /* ---------- Repeat Port as Vector  ---------- */

  // C, Calculate next repeat time by apply the delta to repeat time
  val nextRepeatTimeWire: Option[UInt] =
    if (supportRepeatPort) {
      val result: UInt = WireInit(0.U(VP_REPEAT_FIXPOINT_BITS.W))
      result := (repeatTimeReg.get + deltaRepeatReg.get).apply(VP_REPEAT_FIXPOINT_BITS - 1, 0)
      Some(result)
    } else None

  // C, Integer part of the repeat time
  val ceilRepeatTime: Option[UInt] = if (supportRepeatPort) Some(ceiling(repeatTimeReg.get)) else None

  // C, Wire, indicate the current ceiling repeat time is actually zero
  val zeroRepeatTime: Option[Bool] =
    if (supportRepeatPort) Some(currRepeatTimeReg.get === 0.U || ceilRepeatTime.get === 0.U) else None

  // C, Is Doing Repeating
  val repeating: Option[Bool] = if (supportRepeatPort) Some(currRepeatTimeReg.get > 1.U) else None

  // Whether queue is actual fired, synchronized so OR = AND
  val queueDeqFireExist: Bool = VecInit(queue.vecOutput.map(_.fire())).asUInt().orR()

  /* ---------- Padding/TagState as Vector  ---------- */

  // The start and end of stream, if stream state is not supported, then newStream cannot be used
  val newStream: Bool = if (supportState) readBus.memStreamState.get.StartStr && readBus.memValid else false.B
  val endStream: Bool =
    if (supportState)
      stateQueue.get.deqState.valid && stateQueue.get.deqState.bits.EndStr && stateQueue.get.deqState.bits.End1D &&
      queue.deqFire
    else false.B

  // The candidate state that can possiblely enter the state queue
  val candState: Option[StreamState] =
    if (supportState) Some(StreamState.OR(readBus.memStreamState.get, prevState.get)) else None

  // Padding of current stream
  val padding: UInt = Mux(newStream, readBus.memPadding.getOrElse(noPad), paddingReg.getOrElse(noPad))

  // The padding moment from memory response
  val (memPadZeroNow, memPadOffNow): (Bool, Bool) =
    if (supportPad) {
      val (padZero, padOff): (Bool, Bool) = readBus.memStreamState.get.padNow(padding)
      (readBus.memValid && padZero, readBus.memValid && padOff)
    } else (false.B, false.B)

  // Do padding now from memory response
  val memPadNow: Bool = memPadZeroNow || memPadOffNow

  // The padding moment from state queue
  val (deqPadZeroNow, deqPadOffNow): (Bool, Bool) =
    if (supportPad) {
      val (padZero, padOff): (Bool, Bool) = stateQueue.get.deqState.bits.padNow(padding)
      val stateValid:        Bool = stateQueue.get.deqState.valid
      (stateValid && padZero, stateValid && padOff)
    } else (false.B, false.B)

  // Number of unit in read
  val numUnitInRead: Option[UInt] = if (supportState) Some(PopCount(readBus.memMask)) else None

  // Number of unit needed per vector, unit should be minMemUnitBits
  // This is basically count the number of ACTIVE node * its bitWidth in minMemUnitBits
  val numUnitNeedPerVec: Option[UInt] =
    if (supportState) {
      // Calculate left shift amount, do not do multiplication
      val lshf: Int = log2Ceil(comp2mem)
      // Calculate the number of active data port (drop state port)
      val numActCompPort: UInt = queue.numActCompPort.get
      // Shift and return
      val numNeedCompUnit: UInt = (numActCompPort << lshf).asUInt()
      Some(numNeedCompUnit)
    } else None

  // Number of unit that potentially can be formed as vector, newly unit + leftover unit
  val numCandidateUnit: Option[UInt] = if (supportState) Some(numLeftUnit.get + numUnitInRead.get) else None

  // The leftover unit from candidate
  val numCandLeftUnit: Option[UInt] = if (supportState) Some(numCandidateUnit.get % numUnitNeedPerVec.get) else None

  // The next number of leftover unit
  val nextNumUnitLeftover: Option[UInt] = if (supportState) {
    // Padding will reset the left unit register
    if (supportPad) Some(Mux(memPadNow, 0.U, numCandLeftUnit.get))
    else numCandLeftUnit
  } else None

  // Whether current stream response will result in leftover state
  val leaveVecElem: Option[Bool] = if (supportState) Some(nextNumUnitLeftover.get =/= 0.U) else None

  // Number of element for this enqueue that cannot make a vector
  val numEnqLeft: Option[UInt] = if (supportState) Some((numCandLeftUnit.get >> log2Ceil(comp2mem)).asUInt()) else None

  // Number of vector formed in this memory response
  val numVec: Option[UInt] =
    if (supportState) Some(numCandidateUnit.get / numUnitNeedPerVec.get + {
      // Pad the number of vector covered by this enqueue to state queue
      if (supportPad) Mux(memPadNow && numEnqLeft.get =/= 0.U, 1.U, 0.U) else 0.U
    })
    else None

  // Whether or not can form vector: larger than needed
  val canFormVec: Option[Bool] = if (supportState) Some(numVec.get > 0.U) else None

  /* ------------------------- Combination Logic            ------------------------- */

  // Connect the padded data into multi-io queue
  require(queue.vecInput.length == readBus.memData.length)
  queue.vecInput.zip(readBus.memData).zipWithIndex.foreach { case ((enqPort, tagValue), idx) =>
    require(enqPort.bits.getWidth == tagValue.getWidth, s"It seems to be predication definition inconsistent")
    enqPort.bits := tagValue.asUInt()
    enqPort.valid := readBus.memMask(idx).asBool() && readBus.memValid
  }

  // Connect repeating wire
  queue.repeatDequeue := repeating.getOrElse(false.B)

  // Connect padding wire
  if (supportPad) {
    queue.padZero.get := deqPadZeroNow && stateQueue.get.deqNumLeft =/= 0.U // zero elements left means no padding need
    queue.padOff.get := deqPadOffNow && stateQueue.get.deqNumLeft =/= 0.U
    queue.numPadLeft.get := stateQueue.get.deqNumLeft
  }

  // Connect MultiIO queue ready back to mem read bus
  // Since IVP does not know how many bytes will comes from memory, it has to consider the worst case
  // which is all ports has to be ready
  // Attention: cannot use enqueue fire signal, because it makes the ready depends on valid
  readBus.ivpReady := VecInit(queue.vecInput.map(_.ready)).asUInt().andR()
  readBus.ivpReadyMask := Fill(readBus.ivpReadyMask.getWidth, 1.U(1.W))
  readBus.ivpAvailUnits := queue.left
  readBus.ivpBroadcast match {
    case Some(broadcast) => broadcast := ivpBroadcastReg.getOrElse(false.B)
    case None            =>
  }
  readBus.ivpBroadcastIVPortId match {
    case Some(portId) => portId := broadcastIVPortIdReg.getOrElse(0.U)
    case None         =>
  }

  // Connect the number of vector formed to Stream Stata Queue
  if (supportState) {
    stateQueue.get.enqState.valid := readBus.memValid && canFormVec.getOrElse(false.B) && queue.enqFire
    // If the previous state is not 1D end, new state is 1D end, the OR state will be end masked off
    val maskEndOff:  Bool = !prevState.get.End1D && readBus.memStreamState.get.End1D && leaveVecElem.get
    val maskOrState: StreamState = candState.get.mask(true.B, !maskEndOff)
    // Connect state
    stateQueue.get.enqState.bits := maskOrState
    stateQueue.get.enqState.ready := DontCare // this should be always true, since it is deep enough
    stateQueue.get.enqNumVec := numVec.get
    stateQueue.get.enqNumLeft := numEnqLeft.get
  }

  // Connect to the input synchronization signal of inner queue, since memory read side does not require sync
  // so we connect all sync signal with zero
  queue.inputSyncs.foreach { sync => sync := false.B }

  // If IVP is Xbar based IVP, plesae connect the configuration bitstream to output xbar routing
  if (ivpNode.vpImpl == FullXBarVP) {
    require(queue.outRoute.length == configBits.get.xbarRoute.length)
    queue.outRoute.zip(configBits.get.xbarRoute).foreach { case (route, routeCfg) =>
      require(
        route.getWidth == routeCfg.getWidth,
        s"XBar routing is ${route.getWidth}-bit but config bits has ${routeCfg.getWidth}-bit routing"
      )
      route := routeCfg
    }
  }

  // Connect the dequeue port of state queue to inner queue for output XBar routing
  queue.statedPort match {
    case Some(sPort) =>
      require(stateQueue.isDefined, s"Inner queue has state port but state queue is not defined")
      // Bits
      sPort.bits := stateQueue.get.deqState.bits
      // Valid
      sPort.valid := stateQueue.get.deqState.valid && dequeueFire && !zeroRepeatTime.getOrElse(false.B)
      // Ready
      stateQueue.get.deqState.ready := sPort.ready && dequeueFire && !repeating.getOrElse(false.B)
    case None =>
  }

  /* ------------------------- Finite State Machine         ------------------------- */

  // Set and Reset the doBroadcast bit register
  if (supportBroadcast) {
    when(ivpSetPort.isBroadcastIVPortIdSet) {
      ivpBroadcastReg.get := true.B
      broadcastIVPortIdReg.get := ivpSetPort.vpRegVal
    }.elsewhen(VecInit(memReadPorts.map(x => x.memValid && x.broadcastReset.getOrElse(false.B))).asUInt().orR()) {
      ivpBroadcastReg.get := false.B
    }
  }

  // Update the register that store the state for those unit in queue that have not been formed as vector
  if (supportState) {
    when(queue.enqFire) {
      numLeftUnit.get := nextNumUnitLeftover.get
    }
  }

  // Update the register that hold the padding during the lifttime of stream
  paddingReg match {
    case Some(reg) =>
      when(newStream) {
        reg := readBus.memPadding.get
      }
        // RegNext(endStream) is because SyneReadMem has 1 cycle delay
        .elsewhen(RegNext(endStream)) {
          reg := noPad
        }
    case None =>
  }

  // FSM controls the repeat port
  if (supportRepeatPort) {
    // Set the parameter has highest priority, only update when value is different
    when(ivpSetPort.isRepeatTimeSet && repeatTimeReg.get =/= ivpSetPort.vpRegVal) {
      // Set the repeat time to current repeat time and original repeat time
      currRepeatTimeReg.get := ceiling(ivpSetPort.vpRegVal)
      repeatTimeReg.get := ivpSetPort.vpRegVal
    }.elsewhen(ivpSetPort.isRepeatDeltaSet && deltaRepeatReg.get =/= ivpSetPort.vpRegVal) {
      // Set the stretch to the repeat time, only update when value is different
      deltaRepeatReg.get := ivpSetPort.vpRegVal
    }.otherwise { // Not Configuring, Kick start FSM
      // Stop FSM when #repeat is zero
      when(!zeroRepeatTime.get) {
        // Update FSM when dequeue trigger
        when(dequeueFire && queueDeqFireExist) {
          // If it is last repeat for this vector
          when(currRepeatTimeReg.get === 1.U) {
            // Integer Assign
            currRepeatTimeReg.get := ceiling(nextRepeatTimeWire.get)
            // Fixed Point Assign
            repeatTimeReg.get := nextRepeatTimeWire.get
          }.otherwise {
            // Not the last repeat for this vector
            currRepeatTimeReg.get := currRepeatTimeReg.get - 1.U
          }
        }
      }
    }
  }

  // Update the previous leftover state
  if (supportState) {
    when(canFormVec.get) { // If the current candidate can form vector
      when(leaveVecElem.get) {
        // But there are leftover element, then the "prev state" will be mask off for start
        // since start state is already used
        prevState.get := candState.get.mask(start = false.B, end = true.B)
      }.otherwise {
        // Just match the number of element for a vector, clear the state
        prevState.get := 0.U.asTypeOf(new StreamState)
      }
    }.elsewhen(readBus.memValid) { // If no vector can be formed but we do have memory response, save state
      prevState.get := candState.get
    }
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Report the status of vector port to stream dispatcher
  ivpStatus.alive := ivpBroadcastReg.getOrElse(false.B) || // busy is either broadcasting other port
    readBus.usedByMem || // used by any one of memory
    !queue.empty

  // Report the config status of vector port to stream dispatcher
  ivpStatus.configured := confVP()

  // Report the data availability
  ivpStatus.hasData := !queue.empty

  // Report compute side firing status
  ivpStatus.compFired := dequeueFire

  // Report memory side firing status, since memory can only send response when IVP is ready, so memory valid
  // always means fired.
  ivpStatus.memFired := readBus.memValid

  // Connect doBroadcasting to memory read port
  memReadPorts.foreach { memRead =>
    if (supportBroadcast) {
      require(memRead.ivpBroadcast.isDefined && ivpBroadcastReg.isDefined && broadcastIVPortIdReg.isDefined)
      memRead.ivpBroadcast.get := ivpBroadcastReg.get
      memRead.ivpBroadcastIVPortId.get := broadcastIVPortIdReg.get
    } else {
      // Since memory create bundle based on vp parameter only, but vp may cancel it by taking number
      // of ivp into account, if there is only one ivp, it will not do broadcasting
      // TODO: we need to make the memory node be aware of this: numIVP requires > 1 ==> broadcast
      if (false)
        require(memRead.ivpBroadcast.isEmpty && ivpBroadcastReg.isEmpty && broadcastIVPortIdReg.isEmpty)
    }
  }

  // Connect the vector output of multi-io queue and possible stream state to compute port
  for (compPortIdx <- compParams.indices) {
    val cPort = compPorts(compPortIdx)
    val cPortSync = cPort.ctrl.get.uAct.get
    // Get the output port from queue
    val qPort: DecoupledIO[UInt] = queue.vecOutput(compPortIdx)
    val qSync: Bool = queue.outputSyncs(compPortIdx)
    // Valid, queue port is valid, vector port fired, zero repeat means always consumes
    cPort.valid.get := qPort.valid && dequeueFire && !zeroRepeatTime.getOrElse(false.B)
    // Ready
    qPort.ready := cPort.ready.get
    // Control
    cPort.ctrl.get.dAct.get := cPort.ctrl.get.uAct.get
    // Synchronization, used by this compute node by using node activity
    qSync := cPortSync
    // Bits, data bits from output queue to vector data of compute port
    require(
      cPort.data.get.vecData.getWidth == qPort.bits.getWidth,
      s"Port assign width mismatch, compute data width = ${cPort.data.get.vecData.getWidth}, " +
        s"queue bit width = ${qPort.bits.getWidth}-bit"
    )
    vecDataConnect(cPort.data.get.vecData, qPort.bits)
  }

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */

  // IVP Set Port Width Check
  repeatTimeReg match {
    case Some(repeatFP) =>
      require(
        repeatFP.getWidth > VP_REPEAT_FRAC_BITS,
        s"Number of repeat is fixed point fractional number, " +
          s"$VP_REPEAT_FRAC_BITS-bit for fractional part, and ${repeatFP.getWidth}-bit for integer part"
      )
    case None =>
  }

  /* ------------------------- Utility                      ------------------------- */

  // Ceiling Fixed Point, only keep the integer part of UInt
  def ceiling(fixedPoint: UInt): UInt = {
    require(fixedPoint.getWidth == VP_REPEAT_FIXPOINT_BITS)
    val integer:    UInt = fixedPoint(VP_REPEAT_FIXPOINT_BITS - 1, VP_REPEAT_FRAC_BITS)
    val frac:       UInt = fixedPoint(VP_REPEAT_FRAC_BITS - 1, 0)
    val resultWire: UInt = WireInit(0.U(VP_REPEAT_INT_BITS.W))
    resultWire := Mux(frac.orR(), integer + 1.U, integer)
    resultWire
  }

  // Aggregate all memory read port to create single one memory read port
  def aggMemReadPorts(): IVPMemBus = {
    // Generate Bus Wire
    val readBus: IVPMemBus = WireInit(
      0.U.asTypeOf(
        new IVPMemBus(
          ivpParam = ivpNode,
          busWidth = memWidth,
          busUnitBits = memUnitBits,
          depthBits = memReadParams.map(_.depthBits) max,
          supportPad,
          supportState
        )
      )
    )

    // Whether this ivp is used by any memory nodes
    // TODO: OneHot or all false, check for usedByMem is necessary
    readBus.usedByMem := VecInit(memReadPorts.map(_.usedByMem)).asUInt().orR()
    for (memRead <- memReadPorts) {

      // Connect MEMs <-- IVP Bus
      memRead.ivpReady := readBus.ivpReady
      memRead.ivpReadyMask := readBus.ivpReadyMask
      memRead.ivpAvailUnits := readBus.ivpAvailUnits
      if (supportBroadcast) {
        // Broadcasting cannot be filtered by usedByMem, because it for sure will not be used
        memRead.ivpBroadcast.get := readBus.ivpBroadcast.get
        memRead.ivpBroadcastIVPortId.get := readBus.ivpBroadcastIVPortId.get
      }
      memRead.ivpCapa := finalDepth.U
      memRead.ivpLeftByte := queue.left

      // Connect MEMs --> IVP Bus
      // since only one MEM should have the port at a time, we can do assign as bus
      when(memRead.memValid) {
        readBus.memValid := true.B
        readBus.memMask := memRead.memValidMask
        vecDataConnect(readBus.memData, memRead.memData)
        if (supportState) {
          readBus.memStreamState.get := memRead.memStreamState.getOrElse(0.U.asTypeOf(new StreamState))
        }
        readBus.memPadding match {
          case Some(padding) =>
            if (memRead.memPadding.isDefined) padding := memRead.memPadding.get
            else padding := noPad
          case None =>
        }
        readBus.broadcastReset match {
          case Some(reset) => reset := memRead.broadcastReset.getOrElse(false.B)
          case None        =>
        }
      }
    }
    readBus
  }
}

object InputVectorPortImp extends App {
  var samples = ListBuffer[(Boolean, Boolean, Boolean, Int, Int, Int, Boolean, Int)]()
  println(s"Generating Values")

  for (stated <- Seq(true, false)) {
    for (repeat <- Seq(true, false)) {
      for (broadcast <- Seq(true, false)) {
        for (depth <- Seq(2, 4, 8, 16, 32)) {
          for (input <- 1 until 16) {
            for (output <- 1 until 16) {
              for (padding <- Seq(true, false)) {
                for (busWidth <- Seq(8, 16, 32, 64)) {
                  if (output > 1 || !stated) {
                    if (!(!stated && padding)) {
                      samples += ((stated, repeat, broadcast, depth, input, output, padding, busWidth))
                    }
                  }
                } // End of BusWidth Exploration
              } // End of Padding Exploration
            } // End of Output Exploration
          } // End of Input Exploration
        } // End of Depth Exploration
      } // End of Broadcast Exploration
    } // End of Repeat Exploration
  } // End Of Stated Exploration

  val samplesList = scala.util.Random.shuffle(samples.toList)

  println(s"Total number of configurations: ${samplesList.length}")
  println(s"Start to generate the configurations")

  val iter_number = min(100000, samplesList.length)

  for (n <- 0 to iter_number) {
    val (stated, repeat, broadcast, depth, input, output, padding, busWidth) = samplesList(n)
    println(
      s"Generating configuration $n/$iter_number: stated=$stated, repeat=$repeat, broadcast=$broadcast, depth=$depth, input=$input, output=$output, padding=$padding, busWidth=$busWidth"
    )

    val cdeParam: Parameters = new Config((site, here, up) => {
      case IVPNode =>
        new IVPNodeParameters(
          vpImpl = NonXBarVP,
          nodeId = 0,
          depthByte = depth,
          vpStated = stated,
          repeatedIVP = repeat,
          broadcastIVP = broadcast
        )
      case XLen => 64
    })

    val ivpNode: IVPNodeParameters = cdeParam(IVPNode)
    val reconfParam: Option[CompReconfEdge] =
      if (ivpNode.vpImpl == FullXBarVP) Some(CompReconfEdge(p = cdeParam))
      else None
    // The dummy memory that connect to this IVP
    val memNode: MemNodeParameters =
      MemNodeParameters.DMA.copy(readWidth = busWidth, writeWidth = busWidth, LinearPadding = padding)
    // The dummy compute that connect from this IVP
    val compNode: CompNodeParameters = CompNodeParameters(nodeId = 0, compUnitBits = 64)
    val inParams: Seq[Mem2IVPParameter] = Seq.fill(input)(Mem2IVPParameter(memNode, ivpNode))
    //
    val outParams: Seq[CompDirEdgeParameters] = Seq.fill(output)(new CompDirEdgeParameters(true, compNode, cdeParam))

    val numIVP = 7

    (new ChiselStage).emitVerilog(
      new InputVectorPortImp(ivpNode, reconfParam, inParams, outParams, numIVP)(cdeParam),
      Array(
        "--full-stacktrace",
        "--target-dir",
        "/data/dkupsh/input_vector_port/data/" + input + "-I_" + output + "-O_" + depth + "-D_" + stated + "-S_" + repeat + "-R_" + broadcast + "-B_" + busWidth + "-B_" + padding + "-P"
      )
    )
  }
}
