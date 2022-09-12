package dsagen2.mem.module.stab

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.mem.module.agu.OVPRequestMaskGen
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.diplomacy.DSANodeType.{RecurrenceEngine, RegisterEngine}
import dsagen2.util.RegUtil.RegNextN
import dsagen2.util.UIntUtil.{allOneUInt, groupBitsAs}

/** Stream Table Module
  * 1. keep tracking of the lifetime of stream entry
  * 2. select the next stream entry to AGU
  * 2.1  when select next stream entry to agu, input/output vector port value or status will be combined
  * value will be taken out from ovp for write or indirect update
  * 2.2  receive updated selected stream entry from AGU and update it
  * 3. report the memory status to upper level
  */
class StreamTable(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit p: Parameters)
    extends MultiIOModule
    with BaseStrTable {

  /* ------------------------- Extract Parameters           ------------------------- */

  // Stream Table Select Latency, either 0 or 1
  // 0 : same stream can be issued every cycle
  // 1 : same stream can be issued every other cycle, different can be issued every cycle
  // TODO: we can explore it, 1 gives higher frequency, 0 gives better performance
  val strSelLat: Int = 1
  require(strSelLat == 0 || strSelLat == 1, s"Only support stream table selection every cycle or every other cycle")

  // Number of stage to update Length1D register of OVP
  // TODO: not complete yet, for now can only be 0
  val numUpdOvpL1DStage: Int = 2

  // Number of stage of reporting IVP ready state
  // TODO: not complete yet, for now can only be 0
  val numIvpReadyStage: Int = 0

  /* ------------------------- Derived Parameters           ------------------------- */

  // Count the number of IVP and OVP
  def numIVP: Int = if (memNode.nodeType != RecurrenceEngine) ivpsParam.length else 0

  def numOVP: Int = ovpsParam.length

  def numEntry: Int = {
    val total = numIVP + numOVP
    require(total > 1, s"The total number of vector port $total needs to be larger than 1")
    if (memNode.nodeType != RecurrenceEngine) total else numOVP
  }

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  require(
    (numIVP > 0 || (memNode.isREG || memNode.isDIS || memNode.isREC)) &&
      (numOVP > 0 || (memNode.isGEN && !memNode.supportIndirect && memNode.supportLinear)),
    s"Memory Node ${memNode.getNodeName}, Indirect Feature = ${memNode.numIndirectDimension}, " +
      s"#IVP = $numIVP, #OVP = $numOVP"
  )

  if (memNode.supportIndirect) {
    require(numOVP > 1, s"How you do indirect stream if you only have one output port")
  }

  /* ------------------------- Input / Output               ------------------------- */

  // I, Input, New Stream Entry from Dispatch Bus
  val newStrEntry: StreamEntry = IO(Input(new StreamEntry(memNode, ivpsParam, ovpsParam)))

  // O, Output, Memory Status
  val memStatus: MemoryNodeStatus = IO(Output(new MemoryNodeStatus))

  // I/O, Memory Read Ports, most of signal cannot be known here
  // I, Ignore, memory valid, not the job of stream table
  // I, Ignore, memory valid mask, not the job of stream table
  // I, Ignore, memory tag data, not the job of stream table
  // I, Ignore, broadcast reset, not the job of stream table
  // O, Output, used by memory, to be connect, This is the only signal can be assigned in stream table
  // I, Ignore, memory stream state, not the job of stream table
  val memReadPorts: Seq[MemReadBundle] = ivpsParam.zipWithIndex.map { case (ivpParam, idx) =>
    IO(new MemReadBundle(ivpParam)).suggestName(s"MemoryReadPort$idx")
  }

  // I/O, Memory Write Ports
  // O, ready
  // O, ready mask
  // O, used by memory (taken)
  val memWritePorts: Seq[MemWriteBundle] = ovpsParam.zipWithIndex.map { case (ovpParam, idx) =>
    if (memNode.needOVP) IO(Flipped(new MemWriteBundle(ovpParam))).suggestName(s"MemoryWritePort$idx")
    else WireInit(0.U.asTypeOf(new MemWriteBundle(ovpParam)))
  }

  // Output, Select Stream Entry to be sent to downstream like AGU
  val selStrEntry: StreamEntryWithValues = IO(Output(new StreamEntryWithValues(memNode, ivpsParam, ovpsParam)))

  // Input, Updated Stream Entry to be written into stream table
  val updStrEntry: StreamEntryOutstanding = IO(Input(new StreamEntryOutstanding(memNode, ivpsParam, ovpsParam)))

  // Stream Valid to issue bit vector from outer side
  val outerStrValid: Option[UInt] = if (memNode.isDMA || memNode.isSPM) Some(IO(Input(UInt(numEntry.W)))) else None

  // Stream active bit vector from outer side
  // The reason to have this input is that stream may die in the stream table, but there are pending request that
  // has not enter input vector port (on the flight). These stream are valid, and it should occupy the IVP
  val outerStrAct: Option[UInt] = if (memNode.isDMA || memNode.isSPM) Some(IO(Input(UInt(numIVP.W)))) else None

  // Stream valids output
  val strVlds: UInt = IO(Output(UInt(numEntry.W)))

  // Input, Backpressure pause selection, from ROB or XBar of SPM
  // Read Pause from ROB
  val readPause: Option[Bool] =
    if (memNode.isDMA || memNode.isSPM || memNode.isGEN) Some(IO(Input(Bool()))) else None
  // Write Pause from TLB (if many write request result it too much TLB miss, it may result in pause)
  val writePause: Option[Bool] =
    if (memNode.isDMA || memNode.isIndSPM || memNode.isREG || memNode.isREC) Some(IO(Input(Bool()))) else None
  // Pause from Indirect Banked Scratchpad
  val reqXBarPause: Option[Bool] = if (memNode.isIndSPM) Some(IO(Input(Bool()))) else None
  val rspXBarPause: Option[Bool] = if (memNode.isIndSPM) Some(IO(Input(Bool()))) else None

  /* ------------------------- Registers                    ------------------------- */

  // F, Register Array that holds all stream entry
  // TODO: 1-bit valid is useless, can be optimized out
  val strTable: Vec[StreamEntryOutstanding] = RegInit(
    VecInit(
      Seq.fill(numEntry)(
        0.U.asTypeOf(
          new StreamEntryOutstanding(memNode, ivpsParam, ovpsParam, initRemove = false, portIDRemove = false)
        )
      )
    )
  )

  // F, Register Valid Array that indicate whether each entry is valid
  val strValids: Vec[Bool] = RegInit(VecInit(Seq.fill(numEntry)(false.B)))
  // Name the stream valid based on the main vector port
  for (entryIdx <- 0 until numEntry) {
    val vpName: String = if (entryIdx < numIVP) s"ivp$entryIdx" else s"ovp${entryIdx - numIVP}"
    strValids(entryIdx).suggestName(s"${vpName}_valid")
  }

  // F, Register that holds the vector of tagged data from output vector port
  val ovpsVecData: Seq[Vec[UInt]] =
    Seq.fill(numOVP)(RegInit(VecInit(Seq.fill(memNode.bandwidth)(0.U.asTypeOf(UInt(memNode.memUnitBits.W))))))

  // F, Register that holds the request/responded mask
  // indicating the number of element wanted from ovp
  val ovpsMask: Vec[UInt] =
    if (numOVP > 0) RegInit(VecInit(Seq.fill(numOVP)(0.U(memNode.bandwidth.W))))
    else WireInit(VecInit(Seq.fill(1)(0.U)))

  // F, Register that holds the stream state of each output vector port
  val ovpsStreamState: Option[Vec[StreamState]] = {
    if (memNode.streamStated)
      if (numOVP > 0) Some(RegInit(VecInit(Seq.fill(numOVP)(0.U.asTypeOf(new StreamState)))))
      else Some(WireInit(VecInit(Seq.fill(1)(0.U.asTypeOf(new StreamState)))))
    else None
  }

  // F, Register for each output vector port that indicate whether there is a stream want data from it
  val ovpsReqReg: Vec[Bool] =
    if (numOVP > 0) RegInit(VecInit(Seq.fill(numOVP)(false.B)))
    else WireInit(VecInit(Seq.fill(1)(false.B)))

  // F, Register that holds the valid bit from each output vector port
  val ovpsRspReg: Vec[Bool] =
    if (numOVP > 0) RegInit(VecInit(Seq.fill(numOVP)(false.B)))
    else WireInit(VecInit(Seq.fill(1)(false.B)))

  /* ------------------------- Modules                      ------------------------- */

  /* ------------------------- Wires                        ------------------------- */

  /* ----------- Wire Group that involve in Stream Dispatch/Selection/Update  ----------- */

  // At least one stream entry is valid in stream table
  val atLeastOneStrAlive: Bool = strValids.asUInt().orR()

  // Only one stream active
  val onlyOneStrActive: Bool = PopCount(strValids) === 1.U

  // For performance measure purpose, bool wire for each stream entry that entry is stopped by port
  val strStopByPort: Seq[Bool] = Seq.fill(numEntry)(WireInit(false.B))

  // C, Wire that carry converted new stream entry in form of outstanding stream entry
  val newOutstandingStr: StreamEntryOutstanding =
    WireInit(0.U.asTypeOf(new StreamEntryOutstanding(memNode, ivpsParam, ovpsParam)))

  // C, Wire that indicate new stream enqueue table
  val strDispatched: Seq[Bool] = Seq.fill(numEntry)(WireInit(false.B))

  // C, Wire that aggregate all stream with its dependent values (read stream has no values, so ground to zero)
  // (scheduler.in[...].bits)
  val allStrWithValues: Vec[StreamEntryWithValues] = VecInit(
    Seq.fill(numEntry)(
      WireInit(
        0.U.asTypeOf(
          new StreamEntryWithValues(memNode, ivpsParam, ovpsParam)
        )
      )
    )
  )

  // C, Wire that indicate whether each entry is valid and get all needed value from ovp (writeData, index, L1D, S2D)
  // (scheduler.in[...].valid)
  val str2SelRdy: Seq[Bool] = Seq.fill(numEntry)(WireInit(false.B))

  // C, Wire that indicate each entry is selected (scheduler.in[...].ready)
  // Since AGU is fully combinational, selected also means updated
  val strSelNow: Vec[Bool] = VecInit(Seq.fill(numEntry)(WireInit(false.B)))

  // C, Wire that holds the selected outstanding stream entry (scheduler.out.bits)
  val selStrWithValues: StreamEntryWithValues = WireInit(
    0.U.asTypeOf(new StreamEntryWithValues(memNode, ivpsParam, ovpsParam))
  )

  // C, Wire that indicate selected stream is valid (scheduler.out.valid)
  val selStrValid: Bool = WireInit(false.B)

  // C, Wire that indicates whether or not to accept new stream issue (scheduler.out.ready)
  val run: Bool = WireInit(false.B)

  /* ----------- Wire Group that involve in Data Retrieve from OVPs  ----------- */

  // C, Wire that connects to the write/update part of stream table
  val ovpStrTable: Vec[StreamEntryOutstanding] = {
    // Collect the output vector port part of stream table
    val ovpPart = for (ovpPosition <- numIVP until numEntry) yield {
      strTable(ovpPosition)
    }
    if (numOVP > 0) VecInit(ovpPart)
    else WireInit(VecInit(Seq.fill(1)(0.U.asTypeOf(new StreamEntryOutstanding(memNode, ivpsParam, ovpsParam)))))
  }

  // C, Wire that connect to the write / update part of stream valid
  val ovpStrValids: Vec[Bool] = {
    // Collect the valid bit from stream table
    val ovpPart = for (ovpPosition <- numIVP until numEntry) yield strValids(ovpPosition)
    if (numOVP > 0) VecInit(ovpPart) else WireInit(VecInit(Seq.fill(1)(false.B)))
  }

  // C, Wire that indicate the request/response state of each vector port
  val ovpsReqRspState: Seq[OVPReqRspState] =
    if (numOVP > 0) {
      VecInit(
        for (ovpIdx <- 0 until numOVP) yield {
          val state: OVPReqRspState = WireInit(0.U.asTypeOf(new OVPReqRspState))
          state.responseValid := ovpsRspReg(ovpIdx)
          state.requestReady := ovpsReqReg(ovpIdx)
          state
        }
      )
    } else WireInit(VecInit(Seq.fill(1)(0.U.asTypeOf(new OVPReqRspState))))

  // C, Output Vector Port in Requested state
  val ovpsRequested: Seq[Bool] = ovpsReqRspState.map(_.requested)

  // C, Output Vector Port in Responded state
  val ovpsResponded: Vec[Bool] = VecInit(ovpsReqRspState.map(_.responded))

  // C, Output Vector Port in Responded state
  val ovpsIdle: Seq[Bool] = ovpsReqRspState.map(_.idle)

  // C, Wire that holds index value for each entry
  val entryIdxValues: Option[Vec[UInt]] =
    if (memNode.IndirectIndexStream) Some(WireInit(VecInit(Seq.fill(numEntry)(0.U(memNode.bandBits.W)))))
    else None

  // C, Wire that holds the valid for index value of each entry
  val entryIdxValids: Option[Vec[UInt]] =
    if (memNode.IndirectIndexStream) Some(WireInit(VecInit(Seq.fill(numEntry)(0.U(memNode.bandwidth.W)))))
    else None

  // C, Wire that holds the indirect stride 2D value for each entry
  val entryIndS2DValues: Option[Vec[UInt]] =
    if (memNode.IndirectStride2DStream) Some(WireInit(VecInit(Seq.fill(numEntry)(0.U(memNode.maxIndStride2DBits.W)))))
    else None

  // C, Wire that holds the valid for stride 2D value for each entry
  val entryIndS2DValids: Option[Vec[UInt]] =
    if (memNode.IndirectStride2DStream) Some(WireInit(VecInit(Seq.fill(numEntry)(0.U(1.W)))))
    else None

  // C, Wire that holds the indirect length 1D value for each entry
  val entryIndL1DValues: Option[Vec[UInt]] =
    if (memNode.IndirectLength1DStream) Some(WireInit(VecInit(Seq.fill(numEntry)(0.U(memNode.maxIndLength1DBits.W)))))
    else None

  // C, Wire that holds the indirect length 1D valid for each entry
  val entryIndL1DValids: Option[Vec[UInt]] =
    if (memNode.IndirectLength1DStream) Some(WireInit(VecInit(Seq.fill(numEntry)(0.U(1.W)))))
    else None

  // C, Wire, bit vector for each output vector port that indicate whether there is entry needs index value
  // from current this output vector port: valid, need index from this ovp
  // Map size = #OVP,
  // OVP ID -> [0, 1, 0, 0, 0, 0] (number of entry bit vector that indicate whether need index from this)
  val ovpNeededIndIdxBitvec: Option[Seq[UInt]] =
    if (memNode.IndirectIndexStream) {
      val bitVec: Seq[Vec[Bool]] = for (ovpIdx <- 0 until numOVP) yield {
        // One-hot match to find: valid entry, port match, indirect mode, indirect index required
        val ohMatch: Seq[Bool] = for (entryIdx <- 0 until numEntry) yield {
          // Get the entry
          val entry: StreamEntry = strTable(entryIdx).origStrEntry
          // Indirect OVP Port ID should be defined
          if (numOVP > 1) require(entry.indexOVPortId.isDefined)
          // Valid, Port Match, Indirect Mode, Indirect Index Mode
          strValids(entryIdx) && entry.indexOVPortId.getOrElse(0.U) === ovpIdx.U &&
          entry.LinOrInd.getOrElse(memNode.supportIndirect.B) &&
          entry.indirectIdxStream.getOrElse(memNode.IndirectIndexStream.B)
        }
        // Convert to vector of Bool
        VecInit(ohMatch)
      }
      // Sanity check to make sure that this is a matrix[#OVP][#Entry]
      require(bitVec.length == numOVP && bitVec.forall(_.length == numEntry))
      Some(bitVec.map(_.asUInt()))
    } else None

  // C, Wire that shows the position of entry in total stream table that needs index value for this ovp
  // Map size = #OVP, [ovpId -> entry ID that need indirect index from this OVP]
  val ovpNeededIndIdxEntryIdx: Option[Vec[UInt]] =
    ovpNeededIndIdxBitvec match {
      case Some(bitVec) => Some(VecInit(bitVec.map(OHToUInt(_))));
      case None         => None
    }
  ovpNeededIndIdxEntryIdx match {
    case Some(value) => require(value.length == numOVP);
    case None        =>
  }

  // C, Wire that show there exist entry that want index value from this output vector port
  val ovpNeededIndIdxExist: Option[Vec[Bool]] =
    ovpNeededIndIdxBitvec match {
      case Some(bitVec) => Some(VecInit(bitVec.map(_.orR())));
      case None         => None
    }
  ovpNeededIndIdxExist match {
    case Some(value) => require(value.length == numOVP);
    case None        =>
  }

  // C, Wire, bit vector to find valid entry that need indirect stride 2d from ovp
  val ovpNeededIndS2DBitvec: Option[Seq[UInt]] =
    if (memNode.IndirectStride2DStream) {
      val bitVec: Seq[Vec[Bool]] = for (ovpIdx <- 0 until numOVP) yield {
        // Do valid AND port Id match for each entry
        val ohMatch: Seq[Bool] = for (entryIdx <- 0 until numEntry) yield {
          // Get the entry
          val entry: StreamEntry = strTable(entryIdx).origStrEntry
          // If there is more than one OVP, then indirect stride 2D port should be defined
          if (numOVP > 1) require(entry.stride2DOVPortId.isDefined)
          // Valid, Port ID Match, Indirect Mode, Indirect S2D Mode
          strValids(entryIdx) && entry.stride2DOVPortId.getOrElse(0.U) === ovpIdx.U &&
          entry.LinOrInd.getOrElse(memNode.supportIndirect.B) &&
          entry.indirectS2DStream.getOrElse(memNode.IndirectStride2DStream.B)
        }
        // Convert it to vector of Bool
        VecInit(ohMatch)
      }
      // Sanity check to make sure that this is a matrix[#OVP][#Entry]
      require(bitVec.length == numOVP && bitVec.forall(_.length == numEntry))
      Some(bitVec.map(_.asUInt()))
    } else None

  // C, Wire that shows which the ovp position of stream in the ovp stream table that need indirect stride 2D from it
  val ovpNeededIndS2DEntryIdx: Option[Vec[UInt]] =
    ovpNeededIndS2DBitvec match {
      case Some(bitVec) => Some(VecInit(bitVec.map(OHToUInt(_))));
      case None         => None
    }
  ovpNeededIndS2DEntryIdx match {
    case Some(value) => require(value.length == numOVP);
    case None        =>
  }

  // C, Wire that shows there is an stream entry needs indirect stride 2D from ovp
  val ovpNeededIndS2DExist: Option[Vec[Bool]] =
    ovpNeededIndS2DBitvec match {
      case Some(bitVec) => Some(VecInit(bitVec.map(_.orR())));
      case None         => None
    }
  ovpNeededIndS2DExist match {
    case Some(value) => require(value.length == numOVP);
    case None        =>
  }

  // C, Wire, bit vector for each ovp that shows whether there is an valid stream entry needs indirect length 1D from it
  val ovpNeededIndL1DBitvec: Option[Seq[UInt]] = {
    if (memNode.IndirectLength1DStream) {
      val bitVec: Seq[Vec[Bool]] = for (ovpIdx <- 0 until numOVP) yield {
        // Calculate the valid and port match bit vector for this ovp
        val ohMatch: Seq[Bool] = for (entryIdx <- 0 until numEntry) yield {
          // Get the entry
          val entry: StreamEntry = strTable(entryIdx).origStrEntry
          // If there are more than one OVP, the port id should be defined
          if (numOVP > 1) require(entry.length1DOVPortId.isDefined)
          // Valid, Port ID Match, Indirect Mode, Indirect S2D Mode
          strValids(entryIdx) && entry.length1DOVPortId.getOrElse(0.U) === ovpIdx.U &&
          entry.LinOrInd.getOrElse(memNode.supportIndirect.B) &&
          entry.indirectL1DStream.getOrElse(memNode.IndirectLength1DStream.B)
        }
        // Return as Vector Bool
        VecInit(ohMatch)
      }
      require(bitVec.length == numOVP && bitVec.forall(_.length == numEntry))
      Some(bitVec.map(_.asUInt()))
    } else None
  }

  // C, Wire that shows which other ovp needs indirect length 1D stream from this ovp
  val ovpNeededIndL1DEntryIdx: Option[Vec[UInt]] =
    ovpNeededIndL1DBitvec match {
      case Some(bitVecs) => Some(VecInit(bitVecs.map(OHToUInt(_))));
      case None          => None
    }
  ovpNeededIndL1DEntryIdx match {
    case Some(value) => value.length == numOVP;
    case None        =>
  }

  // C, wire that shows there exist stream in stream table needs indirect length 1D from this port
  val ovpNeededIndL1DExist: Option[Vec[Bool]] =
    ovpNeededIndL1DBitvec match {
      case Some(bitVec) => Some(VecInit(bitVec.map(_.orR())));
      case None         => None
    }
  ovpNeededIndL1DExist match {
    case Some(value) => value.length == numOVP;
    case None        =>
  }

  // C, wire that connects to the data part of output vector port Vec[TagValue] register
  val ovpsData: Vec[UInt] =
    if (numOVP > 0) {
      VecInit({
        ovpsVecData.map { vecData =>
          // Concat the value to be UInt
          val ovpDataBits: UInt = vecData.asUInt()
          // The data bits of ovp should same wide as memory bit width
          require(ovpDataBits.getWidth == memNode.bandBits)
          ovpDataBits
        }
      })
    } else {
      WireInit(VecInit(Seq.fill(1)(0.U)))
    }

  // C, wire that connect to the requesting mask of ovp as valid mask of vector data from ovp
  // since we already know the number of needed element when we request the ovp)
  // it is identical with ovpsReqMask, this is only lower set mask
  // Only when the port reponded, request mask become valid mask
  val ovpsValidMask: Vec[UInt] =
    if (numOVP > 0) VecInit(ovpsMask.zipWithIndex.map { case (mask, ovpIdx) => Mux(ovpsResponded(ovpIdx), mask, 0.U) })
    else WireInit(VecInit(Seq.fill(1)(0.U)))

  val ovpsReadyMask: Seq[UInt] =
    ovpsMask.zipWithIndex.map { case (mask, ovpIdx) => Mux(ovpsRequested(ovpIdx), mask, 0.U) }

  // C, wire, bit for each output vector port that indicate whether there is ovp entry need data from it
  // OVP can be needed by write data, indirect index, indirect stride2d, indirect length 1d, 4 kinds
  // needed by write is indicated by same position entry valid
  // needed by indirect value is pre-computed
  // For each ovp : Seq[neededForWrite, neededForIndex, neededForStride2D, neededForLength1D]
  //                    0             , 1             , 2                , 3
  val ovpsNeededBitVec: Seq[(Bool, Option[Bool], Option[Bool], Option[Bool])] = {
    for (ovpIdx <- 0 until numOVP) yield {

      // Whether this ovp is needed by write / update stream
      val writeNeeded: Bool = ovpStrValids(ovpIdx)

      // Whether this ovp data is needed by Indirect L1D write / update stream
      val writeNeededByIndL1D: Bool = ovpStrTable(ovpIdx).origStrEntry.indirectL1DStream
        .getOrElse(memNode.IndirectLength1DStream.B)

      // Whether Indirect L1D (the indirect length 1D mentioned above) is valid
      // Because the ovp data request is based on correct L1D (which is indirect), so this will block
      // the request to OVP until the correct (indirect) L1D is valid
      val writeNeededIndL1DValid: Bool = entryIndL1DValids match {
        case Some(indL1DVlds) => indL1DVlds(numIVP + ovpIdx).asBool()
        case None             => true.B // no indirect l1d mean it is linear, which is always valid
      }

      // Request this vector port for write data
      val writeRequest: Bool = writeNeeded && (!writeNeededByIndL1D || writeNeededIndL1DValid)

      // Calculate whether this ovp is needed by a indirect index (1D) stream
      val indexNeeded: Option[Bool] = if (memNode.IndirectIndexStream) {
        require(ovpNeededIndIdxExist.isDefined)
        Some(ovpNeededIndIdxExist.get.apply(ovpIdx))
      } else None

      // If this ovp is needed by indirect index stream, which is also a indirect L1D stream
      val indexNeededByIndL1D: Option[Bool] = if (memNode.IndirectLength1DStream) {
        if (memNode.IndirectIndexStream) {
          require(ovpNeededIndIdxEntryIdx.isDefined)
          // Get the entry id that requires this ovp for indirect index first
          val entryIdxNeedIndIdx: UInt = ovpNeededIndIdxEntryIdx.get.apply(ovpIdx)
          // Check if this entry (that asks indirect index from this ovp) is an indirect L1D stream as well
          Some(strTable(entryIdxNeedIndIdx).origStrEntry.indirectL1DStream.getOrElse(memNode.IndirectLength1DStream.B))
        } else None
      } else None

      // If this ovp is needed by indirect index + indirect L1D stream,
      // index request should be false until indirect L1D is valid
      val indexNeededIndL1DValid: Option[Bool] = entryIndL1DValids match {
        case Some(indL1DValids) =>
          if (memNode.IndirectIndexStream) {
            require(ovpNeededIndIdxEntryIdx.isDefined)
            // Get the ovpEntryId that requires this ovp for index first
            val entryIdxNeedIndIdx: UInt = ovpNeededIndIdxEntryIdx.get.apply(ovpIdx)
            require(indL1DValids.head.getWidth == 1, s"Valid should be 1-bit")
            Some(indL1DValids(entryIdxNeedIndIdx).asBool())
          } else None // even no indirect index supported, so no entry will request ovp for index
        case None => None
      }

      // Request this vector port for index data
      val indexRequest: Option[Bool] = if (memNode.IndirectIndexStream) {
        (indexNeeded, indexNeededByIndL1D, indexNeededIndL1DValid) match {
          case (Some(needed), Some(neededByIndL1D), Some(neededByIndL1DValid)) =>
            // This OVP is required for index AND the stream asks for index is not a indirect L1D stream,
            // if it is a indirect L1D stream, we need to wait until indirect L1D is valid
            Some(needed && (!neededByIndL1D || neededByIndL1DValid))
          case (Some(needed), None, None) => Some(needed)
          case (None, None, None)         => None
          case _                          => require(requirement = false, s"I think the rest case should be illegal"); None
        }
      } else None

      // Calculate needed by indirect stride 2D, since we always request l1d = 1, so no need to check length1d valid
      // TODO: we might need to review it if our AGU can do multiple l1d request
      val stride2DNeeded: Option[Bool] = if (memNode.IndirectStride2DStream) {
        require(ovpNeededIndS2DExist.isDefined)
        Some(ovpNeededIndS2DExist.get.apply(ovpIdx))
      } else None

      // Calculate needed by indirect length 1D since we always request l1d = 1, so no need to check length1d valid
      // TODO: we might need to review it if our AGU can do multiple l1d request
      val length1DNeeded: Option[Bool] = if (memNode.IndirectLength1DStream) {
        require(ovpNeededIndL1DExist.isDefined)
        Some(ovpNeededIndL1DExist.get.apply(ovpIdx))
      } else None

      // Return
      (writeRequest, indexRequest, stride2DNeeded, length1DNeeded)
    }
  }

  // C, wire, calculate the request mask for each entry
  // IVP Entry : indirect index, indirect L1D, indirect S2D
  // OVP Entry : write data, indirect index, indirect L1D, indirect S2D
  // we only need to calculate the mask if this entry is requesting write data or index to ovp
  val entryRequestMasks: Seq[(UInt, Option[UInt], Option[UInt], Option[UInt])] = {
    for (entryIdx <- 0 until numEntry) yield {
      // Get the entry from the ovp stream table
      val strEntry: StreamEntryOutstanding = strTable(entryIdx)
      // Get Address
      val address: UInt = strEntry.currStartPoint1D.getOrElse(
        strEntry.origStrEntry.startPoint.getOrElse(0.U(log2Ceil(memNode.bandwidth).W))
      )
      // Make sure that the address is reasonable
      if (address.getWidth < 2) {
        require(
          requirement = false,
          s"currsp1d = ${strEntry.currStartPoint1D.isDefined}, " +
            s"currsp1d.width = ${strEntry.currStartPoint1D.getOrElse(0.U).getWidth}," +
            s"oriSp1d = ${strEntry.origStrEntry.startPoint.isDefined}, " +
            s"oriSp1d.width = ${strEntry.origStrEntry.startPoint.getOrElse(0.U).getWidth}"
        )
      }

      // Get the linear Length 1D
      val linearL1D: UInt = strEntry.currLength1Din1D.getOrElse(strEntry.origStrEntry.initLength1D.getOrElse(0.U))

      // Find the correct length 1D
      val length1D: UInt =
        if (memNode.IndirectLength1DStream) {
          // Sanity check
          require(entryIndL1DValues.isDefined)
          // Get the indirect length 1D
          require(entryIndL1DValues.get.head.getWidth >= memNode.maxIndLength1DBits)
          val initIndL1D: UInt = entryIndL1DValues.get.apply(entryIdx).apply(memNode.maxIndLength1DBits - 1, 0)
          // Get the linear/indirect mode of Length1D
          val indirectL1DMode: Bool =
            strEntry.origStrEntry.indirectL1DStream.getOrElse(memNode.IndirectLength1DStream.B)
          // Select between indirect L1D and Linear L1D
          Mux(indirectL1DMode, initIndL1D, linearL1D)
        } else {
          linearL1D
        }

      // If the stream is running at DMA or GEN Indirect Index Stream Mode, length 1D for write and index should always be 1
      val oneIndIndexNeeded: Bool = (memNode.isDMA || memNode.isGEN).B &&
        strEntry.origStrEntry.indirectIdxStream.getOrElse(memNode.IndirectIndexStream.B)

      // Data Type Exp for up to four kinds of request and Stride1D
      val memDataTypeExp: Option[UInt] = strEntry.origStrEntry.memDataTypeExp
      val idxDataTypeExp: Option[UInt] = strEntry.origStrEntry.idxStrDataType
      val s2dDataTypeExp: Option[UInt] = strEntry.origStrEntry.s2dStrDataType
      val l1dDataTypeExp: Option[UInt] = strEntry.origStrEntry.l1dStrDataType
      val stride1D:       Option[UInt] = strEntry.origStrEntry.stride1D

      // Calculate the final length 1d with the consideration of indirect DMA
      val finalL1D: UInt = Mux(
        oneIndIndexNeeded,
        // If runs under DMA Indirect Index Stream Enabled mode, length 1D should be one
        Mux(length1D.orR(), 1.U(length1D.getWidth.W), 0.U(length1D.getWidth.W)),
        length1D
      )

      // Generate requesting mask for write data (this is necessary)
      val (_, writeReqMask): (Any, UInt) = {
        // Only OVP part of stream table will request for write data
        if (memNode.nodeType != RegisterEngine && entryIdx >= numIVP) {
          OVPRequestMaskGen(
            address = address,
            length1D = finalL1D,
            dataTypeExp = memDataTypeExp,
            stride1D = stride1D,
            memWidth = memNode.bandwidth
          )
        } else {
          (null, allOneUInt(memNode.writeWidth))
        }
      }

      // Generate request mask for indirect index stream
      val idxReqMask: Option[UInt] = {
        if (memNode.IndirectIndexStream) {
          val (_, idxMask) = OVPRequestMaskGen(
            address = address,
            length1D = finalL1D,
            dataTypeExp = idxDataTypeExp,
            stride1D = stride1D,
            memWidth = memNode.bandwidth
          )
          Some(idxMask)
        } else None
      }

      // Generate request mask for indirect stride2d stream
      val s2dReqMask: Option[UInt] = {
        if (memNode.IndirectStride2DStream) {
          val (_, s2dMask) = OVPRequestMaskGen(
            address = address,
            length1D = 1.U(log2Ceil(memNode.bandwidth).W),
            dataTypeExp = s2dDataTypeExp,
            stride1D = stride1D,
            memWidth = memNode.bandwidth
          )
          Some(s2dMask)
        } else None
      }

      // Generate request mask for indirect length1d stream
      val l1dReqMask: Option[UInt] = {
        if (memNode.IndirectLength1DStream) {
          val (_, l1dMask) = OVPRequestMaskGen(
            address = address,
            length1D = 1.U(log2Ceil(memNode.bandwidth).W),
            dataTypeExp = l1dDataTypeExp,
            stride1D = stride1D,
            memWidth = memNode.bandwidth
          )
          Some(l1dMask)
        } else None
      }

      // Return
      (writeReqMask, idxReqMask, s2dReqMask, l1dReqMask)
    }
  }

  // C, wire, ovp write request mask, the first numIVP is useless
  val ovpWriteReqMask: Seq[UInt] = entryRequestMasks.map(_._1).drop(numIVP)

  // C, wire, ovp index request mask
  val ovpIndexReqMask: Option[Seq[UInt]] = if (numOVP > 0 && entryRequestMasks.head._2.isDefined) {
    // Map and VecInit the index request mask for hardware index
    val entryIdxReqMask: Vec[UInt] =
      if (numOVP > 0) VecInit(entryRequestMasks.map(_._2.get)) else WireInit(VecInit(Seq.fill(numEntry)(0.U)))
    // Use pre-generated entryIdx to access it
    require(ovpNeededIndIdxEntryIdx.isDefined)
    require(entryIdxReqMask.length == numEntry)
    // Loop all ovp
    val idxReqMasks: Seq[UInt] = for (ovpIdx <- 0 until numOVP) yield {
      // Get the entry idx that asks for index value
      val entryIdxAskForIndex: UInt = ovpNeededIndIdxEntryIdx.get.apply(ovpIdx)
      // Use it to access the mask
      entryIdxReqMask(entryIdxAskForIndex)
    }
    Some(idxReqMasks)
  } else None

  // C, wire, ovp indirect stride 2D request mask
  val ovpStride2DReqMask: Option[Seq[UInt]] = if (numOVP > 0 && memNode.IndirectStride2DStream) {
    // Sanity check
    require(entryRequestMasks.head._3.isDefined)
    // Map and VecInit the index request mask for hardware index
    val entryS2DReqMask: Vec[UInt] =
      if (numOVP > 0) VecInit(entryRequestMasks.map(_._3.get)) else WireInit(VecInit(Seq.fill(numEntry)(0.U)))
    // Use pre-generated entryIdx to access it
    require(ovpNeededIndS2DEntryIdx.isDefined)
    require(entryS2DReqMask.length == numEntry)
    // Loop all ovp
    val s2dReqMasks: Seq[UInt] = for (ovpIdx <- 0 until numOVP) yield {
      // Get the entry idx that asks for stride2d value
      val entryIdxNeedS2D: UInt = ovpNeededIndS2DEntryIdx.get.apply(ovpIdx)
      // Use it to access mask
      entryS2DReqMask(entryIdxNeedS2D)
    }
    Some(s2dReqMasks)
  } else None

  // C, wire, ovp indirect length1d request mask
  val ovpLength1DReqMask: Option[Seq[UInt]] = if (numOVP > 0 && memNode.IndirectLength1DStream) {
    // Sanity check
    require(entryRequestMasks.head._4.isDefined)
    // Map and VecInit the index request mask for hardware index
    val entryL1DReqMask: Vec[UInt] =
      if (numOVP > 0) VecInit(entryRequestMasks.map(_._4.get)) else WireInit(VecInit(Seq.fill(numEntry)(0.U)))
    // Use pre-generated entryIdx to access it
    require(ovpNeededIndL1DEntryIdx.isDefined)
    require(entryL1DReqMask.length == numEntry)
    // Loop all ovp
    val l1dReqMasks: Seq[UInt] = for (ovpIdx <- 0 until numOVP) yield {
      // Get the entry idx that asks for length1d value
      val entryIdxNeedL1D: UInt = ovpNeededIndL1DEntryIdx.get.apply(ovpIdx)
      // Use it to access mask
      entryL1DReqMask(entryIdxNeedL1D)
    }
    Some(l1dReqMasks)
  } else None

  // C, wire, whether ovp is needed by any valid entry for any reason
  val ovpNeeded: Seq[Bool] = for (ovpIdx <- 0 until numOVP) yield {
    // Get the Idle to Request Trigger Signal
    val (writeNeeded, idxNeeded, s2dNeeded, l1dNeeded) = ovpsNeededBitVec(ovpIdx)
    writeNeeded || idxNeeded.getOrElse(false.B) || s2dNeeded.getOrElse(false.B) || l1dNeeded.getOrElse(false.B)
  }

  // C, wire, calculate the memory operation that each stream entry is doing for memory status report
  //           MSB   [Atom5, Atom4,Atom3,Atom2,Atom1,Atom0,wr  , rd   ] LSB
  val strMemOp: Seq[UInt] = {
    for (entryIdx <- 0 until numEntry) yield {
      // valid
      val valid: Bool = strValids(entryIdx)
      // Doing Atomic Operation 5
      val aop5: Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpAtomOp5
      val aop4: Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpAtomOp4
      val aop3: Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpAtomOp3
      val aop2: Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpAtomOp2
      val aop1: Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpAtomOp1
      val aop0: Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpAtomOp0
      val wr:   Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpWrite
      val rd:   Bool = strTable(entryIdx).origStrEntry.memOperation.getOrElse(memOpRead) === memOpRead
      Cat(aop5, aop4, aop3, aop2, aop1, aop0, wr, rd) & Fill(8, valid)
    }
  }

  // Wire, the current selected stream reach the end of stream or end of 1D stream
  val end1DorDead: Bool = (!updStrEntry.origStrEntry.valid) || (updStrEntry.origStrEntry.valid && updStrEntry.StartOf1D)

  /* ------------------------- Combination Logic            ------------------------- */

  // Connect from stream dispatcher bus to new outstanding stream entry that going to be put into stream table
  disp2table(newStrEntry = newStrEntry, outstandStr = newOutstandingStr)

  // Construct stream with all needed values
  for (entryIdx <- 0 until numEntry) {
    // Get the wire of it
    val strWithValues: StreamEntryWithValues = allStrWithValues(entryIdx)
    // Connect it to stream entry in the table
    strWithValues.currStrEntry.origStrEntry.targetLocalPortId match {
      case Some(portIdWire) =>
        if (entryIdx < numIVP) {
          portIdWire := entryIdx.U
        } else {
          portIdWire := (entryIdx - numIVP).U
        }
      case None =>
    }
    // TODO: since I remove init and port id in stream table, this may be problematic
    strWithValues.currStrEntry := strTable(entryIdx)
    // Connect the optional index values
    strWithValues.indIdxValues match {
      case Some(idxValueWire) =>
        require(entryIdxValues.isDefined)
        val idxValue: UInt = entryIdxValues.get.apply(entryIdx)
        if (memNode.isSPM) {
          val idxValueGroup: Seq[UInt] = groupBitsAs(idxValue, idxValueWire.head.getWidth)
          require(idxValueWire.length == idxValueGroup.length)
          idxValueWire.zip(idxValueGroup).foreach { case (idx, idxValue) =>
            require(idxValue.getWidth >= idx.getWidth)
            idx := idxValue
          }
        } else if (memNode.isDMA || memNode.isGEN) {
          // For indirect index DMA, every time, it can only request one
          require(idxValueWire.length == 1)
          idxValueWire.head := idxValue // Only connects the lower bits
        } else {
          require(requirement = false, s"I believe only DMA and SPM can support indirect index")
        }
      case None =>
    }
    // Connect the Indirect Index Value Valid, Indirect Index DMA has special case for it
    if (memNode.IndirectIndexStream) {
      val indIdxValid: Vec[Bool] = strWithValues.indIdxValids.get
      // For Indirect Index DMA, we should just use ovpResponded[entry.indirectIndexPortId]
      if (memNode.isDMA || memNode.isGEN) {
        require(indIdxValid.length == 1, s"I believe Indirect Index DMA's valid bit is just 1")
        // Get the ovp port number that provides index stream for this entry
        val indIdxOVPortId: UInt = strTable(entryIdx).origStrEntry.indexOVPortId.getOrElse(0.U)
        // If the index ovp is responded, then it is ready
        indIdxValid.head := ovpsResponded(indIdxOVPortId)
      } else if (memNode.isSPM) {
        // For scratchpad, it should connect to the entryIdxValids
        require(entryIdxValids.isDefined)
        val idxValid: UInt = entryIdxValids.get.apply(entryIdx)
        require(indIdxValid.length == idxValid.getWidth)
        indIdxValid.zip(idxValid.asBools()).foreach { case (validWire, validFromOVP) =>
          validWire := validFromOVP
        }
      } else {
        require(requirement = false, s"I believe only DMA and SPM support index stream")
      }
    }
    // Connect the indirect stride2D value
    strWithValues.indS2DValue match {
      case Some(s2dValue) =>
        require(entryIndS2DValues.isDefined)
        // Connect the lower bits automatically
        s2dValue := entryIndS2DValues.get.apply(entryIdx)
      case None =>
    }
    // Connect the indirect stride2D valid, use ovpResponded
    strWithValues.indS2DValid match {
      case Some(s2dValid) =>
        // Get the ovp port id for stride2D, use the responded signal of it to indicate the valid of s2d
        val indS2DOVPortId: UInt = strTable(entryIdx).origStrEntry.stride2DOVPortId.getOrElse(0.U)
        s2dValid := ovpsResponded(indS2DOVPortId)
      case None =>
    }
    // Connect the indirect length1D value
    strWithValues.indL1DValue match {
      case Some(l1dValue) =>
        require(entryIndL1DValues.isDefined)
        //Connect the lower bits
        l1dValue := entryIndL1DValues.get.apply(entryIdx)
      case None =>
    }
    // Connect the indirect length1D valid
    strWithValues.indL1DValid match {
      case Some(l1dValid) =>
        // Get the ovp port id that provides indirect length 1d
        val indL1DOVPortId: UInt = strTable(entryIdx).origStrEntry.length1DOVPortId.getOrElse(0.U)
        l1dValid := ovpsResponded(indL1DOVPortId)
      case None =>
    }
    // Connect the write tag data and valid
    if (entryIdx >= numIVP) {
      val ovpEntryIdx: Int = entryIdx - numIVP
      // Connect the vector of tagged value
      strWithValues.writeVecData match {
        case Some(vecTagValue) =>
          // Get the vector of tag value directly from register
          val vecDataReg: Vec[UInt] = ovpsVecData(ovpEntryIdx)
          // Connect with ovp data of the same position
          require(vecTagValue.length == vecDataReg.length)
          // Connect for each
          vecTagValue.zip(vecDataReg).foreach { case (wire, reg) =>
            require(wire.getWidth == reg.getWidth)
            wire := reg
          }
        case None =>
      }
      // Connect the valids
      strWithValues.writeValids match {
        case Some(writeValid) =>
          // Get the mask from ovp
          val ovpMask: UInt = ovpsMask(ovpEntryIdx)
          // Check width
          require(writeValid.length == ovpMask.getWidth)
          writeValid.zipWithIndex.foreach { case (bool, i) =>
            bool := ovpMask(i).asBool()
          }
        case None =>
      }
    }
  }

  // Connect stream's ready for issue to AGU
  for (entryIdx <- 0 until numEntry) {
    // Get the target wire
    val strReadyWire: Bool = str2SelRdy(entryIdx)

    // Get the stop by port wire for performance measure
    val stopByPort: Bool = strStopByPort(entryIdx)

    // Self Valid : First stream needs to be valid
    val strValid: Bool = strValids(entryIdx) && strTable(entryIdx).origStrEntry.valid

    // Port Type and Memory Type Valid
    // All dependent port are responded (write) or ready (read)
    // Calculate port valid
    val portValid: Bool = {
      // Read Stream Ready combine ivp ready and not ROB pause
      if (entryIdx < numIVP) {
        // Calculate whether it is being broadcasted, if it is , then the other port needs to be ready
        val broadCastReady: Bool = {
          // Bit vector to check whether each other port is under broadcast mode and their broadcast id points to this
          val bitVec: Seq[Bool] = for (otherIvpIdx <- 0 until numIVP) yield {
            if (otherIvpIdx == entryIdx) {
              true.B // self ready is check below, cannot broadcast it self
            } else {
              memReadPorts(otherIvpIdx).ivpBroadcast match {
                case Some(broadcast) =>
                  Mux(
                    broadcast && memReadPorts(otherIvpIdx).ivpBroadcastIVPortId.get === entryIdx.U,
                    // If it is under broadcast mode and do broadcast this ivp, then wait for it valid
                    memReadPorts(otherIvpIdx).ivpReady,
                    true.B
                  )
                case None => true.B // broadcast not supported, do not check
              }
            }
          }
          VecInit(bitVec).asUInt().andR()
        }
        val portReady: Bool = memReadPorts(entryIdx).ivpReady && broadCastReady
        /*RegNextN(portReady, numIvpReadyStage, List(portReady)).last &&*/
        !readPause.getOrElse(false.B)
      }
      // Write Stream Ready combine ovp responded and not write pause
      else {
        ovpsResponded(entryIdx - numIVP) && !writePause.getOrElse(false.B)
      }
    }

    // Indirect Stream Valid
    // Check whether index port responded
    val indexPortValid: Bool = {
      // Get index port id
      val idxOVPortID: UInt = strTable(entryIdx).origStrEntry.indexOVPortId.getOrElse(0.U)
      Mux(
        strTable(entryIdx).origStrEntry.indirectIdxStream.getOrElse(memNode.IndirectIndexStream.B),
        ovpsResponded(idxOVPortID),
        true.B
      )
    }
    // Check whether stride 2D port responded
    val s2dOVPortValid: Bool = {
      // Get stride port id
      val s2dOVPortId: UInt = strTable(entryIdx).origStrEntry.stride2DOVPortId.getOrElse(0.U)
      Mux(
        strTable(entryIdx).origStrEntry.indirectS2DStream.getOrElse(memNode.IndirectStride2DStream.B),
        ovpsResponded(s2dOVPortId),
        true.B
      )
    }
    // Check whether length 1D port responded
    val l1dOVPortValid: Bool = {
      // Get stride port id
      val l1dOVPortId: UInt = strTable(entryIdx).origStrEntry.length1DOVPortId.getOrElse(0.U)
      Mux(
        strTable(entryIdx).origStrEntry.indirectL1DStream.getOrElse(memNode.IndirectLength1DStream.B),
        ovpsResponded(l1dOVPortId),
        true.B
      )
    }

    // Get the whether current stream is indirect stream -- indirect stream cannot do continuous issue
    val isIndStr: Bool = strTable(entryIdx).origStrEntry.LinOrInd.getOrElse(memNode.supportIndirect.B)

    // Valid from outer scheduler
    val outerValid: Bool = outerStrValid match {
      case Some(value) => value.asBools().apply(entryIdx)
      case None        => true.B
    }

    // Stream validness
    val streamValid: Bool = {
      // 1-cycle issue stream table should switch between input/write vector port stream entry
      // read stream can be issued in non-stop way
      // write stream cannot be issued in a continue way: 2 cycle delay (idle -> request -> respond) to get data
      if (strSelLat > 0) {
        if (entryIdx < numIVP) {
          // continue issue of read stream, stop by stream end
          // indirect stream cannot do continuous issue
          (strValid && (!strSelNow(entryIdx) || onlyOneStrActive)) && (!isIndStr || !strSelNow(entryIdx))
        } else {
          // write stream cannot perform non-stop issue, will be stopped by being selected now
          strValid && !strSelNow(entryIdx)
        }
      } else strValid // for 0 cycle stream table, stream entry valid should be enough
    }

    // Combine dependent data readiness
    strReadyWire := streamValid && outerValid && portValid && indexPortValid && s2dOVPortValid && l1dOVPortValid
    // Stream issue is stopped by port if stream is not valid or valid stream does not meet any port condition
    stopByPort := !streamValid || (streamValid && !(portValid && indexPortValid && s2dOVPortValid && l1dOVPortValid))
    strReadyWire.suggestName(s"entry${entryIdx}_ReadyForSelect")
  }

  // Connect downward backpressure
  run := !reqXBarPause.getOrElse(false.B) && !rspXBarPause.getOrElse(false.B)

  // Connect the Stream Selection Scheduler
  {
    val (tValid, tReadies, tChosen): (Bool, Seq[Bool], UInt) = StreamTable.RRStreamSelection(memNode, str2SelRdy, run)
    // sanity check
    require(strSelNow.length == tReadies.length)
    // Pipeline stage
    val readies: Seq[Bool] = tReadies.map(x => RegNextN(x, strSelLat, List(x)).last)
    val chosen:  UInt = RegNextN(tChosen, strSelLat, List(tChosen)).last
    val valid:   Bool = RegNextN(tValid, strSelLat, List(tValid)).last
    // Connect
    selStrWithValues := allStrWithValues(chosen)
    selStrValid := valid && strValids(chosen)
    strSelNow.zip(readies).zipWithIndex.foreach { case ((sel, ready), entryIdx) =>
      sel := ready && valid && entryIdx.U === chosen
      sel.suggestName(s"str${entryIdx}_Selected")
    }
  }

  // Calculate whether the entry is dispatched
  for (entryIdx <- 0 until numEntry) {
    if (entryIdx < numIVP) {
      // Dispatch to IVP Part
      strDispatched(entryIdx) := newStrEntry.valid &&
        newStrEntry.memOperation.getOrElse(memOpRead) === memOpRead &&
        newStrEntry.targetLocalPortId.getOrElse(0.U) === entryIdx.U
    } else {
      // Dispatch to OVP Part
      strDispatched(entryIdx) := newStrEntry.valid &&
        newStrEntry.memOperation.getOrElse({
          if (memNode.isREC) memOpWrite else memOpRead
        }) =/= memOpRead &&
        newStrEntry.targetLocalPortId.getOrElse(0.U) === (entryIdx - numIVP).U
    }
  }

  // Connect the Indirect Index Value/Valid/Predication wire from OVP for each ovp entry
  indirectConnectionFromOVP(entryIdxValues, entryIdxValids, strTable.map(_.origStrEntry.indexOVPortId))

  // Connect the Indirect Stride 2D Value/Valid/Predication wire from OVP for each ovp entry
  indirectConnectionFromOVP(
    entryIndS2DValues,
    entryIndS2DValids,
    strTable.map(_.origStrEntry.stride2DOVPortId),
    connectValid = false
  )

  // Connect the Indirect Stride 2D Value/Valid/Predication wire from OVP for each ovp entry
  indirectConnectionFromOVP(
    entryIndL1DValues,
    entryIndL1DValids,
    strTable.map(_.origStrEntry.length1DOVPortId),
    connectValid = false
  )

  /* ------------------------- Finite State Machine         ------------------------- */

  /* ----------- Stream Dispatch/Selection/Update  ----------- */

  for (entryIdx <- 0 until numEntry) {
    // Dispatch has the highest priority
    when(strDispatched(entryIdx)) {
      strTable(entryIdx) := newOutstandingStr
      strValids(entryIdx) := newOutstandingStr.origStrEntry.valid
    }.elsewhen(strSelNow(entryIdx)) {
      strTable(entryIdx) := updStrEntry
      strValids(entryIdx) := updStrEntry.origStrEntry.valid
    }.otherwise {
      /* Stream Entry keeps the same, maybe waiting for data*/
    }
  }

  /* ----------- Data Retrieve from OVPs  ----------- */

  // Each Output Vector Port has its own FSM to switch between state
  // State : Idle -> Requested -> Responded
  for (ovpIdx <- 0 until numOVP) {

    // Get the Idle to Request Trigger Signal
    val (writeNeeded, indexNeeded, stride2DNeeded, length1DNeeded) = ovpsNeededBitVec(ovpIdx)
    // Get write requesting mask
    val writeReqMask: UInt = ovpWriteReqMask(ovpIdx)
    // Get index request mask
    val indexReqMask: Option[UInt] =
      ovpIndexReqMask match {
        case Some(value) => Some(value(ovpIdx));
        case None        => None
      }
    // Get the index dependent entry selected signal
    val idxDepSelected: Option[Bool] = ovpIndexReqMask match {
      case Some(_) =>
        // Make sure the ovpIdx -> index entryId mapping is defined
        require(ovpNeededIndIdxExist.isDefined && ovpNeededIndIdxEntryIdx.isDefined && ovpNeededIndIdxBitvec.isDefined)
        require(ovpNeededIndIdxExist.get.length == numOVP && ovpNeededIndIdxEntryIdx.get.length == numOVP)
        // Get the index of entry that ask this OVP for indirect index
        val needed:   Bool = ovpNeededIndIdxExist.get.apply(ovpIdx)
        val entryIdx: UInt = ovpNeededIndIdxEntryIdx.get.apply(ovpIdx)
        require(entryIdx.getWidth == log2Ceil(numEntry) && strSelNow.length == numEntry)
        // Needed and selected
        Some(needed && strSelNow(entryIdx))
      case None => None
    }
    // Get stride2D request mask
    val stride2DReqMask: Option[UInt] =
      ovpStride2DReqMask match {
        case Some(value) => Some(value(ovpIdx));
        case None        => None
      }
    // Get the indirect stride 2D dependent entry selected signal
    // This signal is only used to switch the OVP request state from respond to idle
    // so it got switched when dependent stream dead or new 1D stream is next
    val s2dDepSelected: Option[Bool] = ovpStride2DReqMask match {
      case Some(_) =>
        // Make sure the ovpIdx -> stride 2D entry Id mapping is defined
        require(ovpNeededIndS2DExist.isDefined && ovpNeededIndS2DEntryIdx.isDefined && ovpNeededIndS2DBitvec.isDefined)
        require(ovpNeededIndS2DExist.get.length == numOVP && ovpNeededIndS2DEntryIdx.get.length == numOVP)
        // Get the index of entry that ask this OVP for indirect stride 2D
        val needed:   Bool = ovpNeededIndS2DExist.get.apply(ovpIdx)
        val entryIdx: UInt = ovpNeededIndS2DEntryIdx.get.apply(ovpIdx)
        require(entryIdx.getWidth == log2Ceil(numEntry) && strSelNow.length == numEntry)
        // Needed, selected, end of 1D or dead stream
        Some(needed && strSelNow(entryIdx) && end1DorDead)
      case None => None
    }
    // Get length1D request mask
    val length1DReqMask: Option[UInt] =
      ovpLength1DReqMask match {
        case Some(value) => Some(value(ovpIdx));
        case None        => None
      }
    // Get the indirect length 1D dependent entry selected signal
    val l1dDepSelected: Option[Bool] = ovpLength1DReqMask match {
      case Some(_) =>
        // Make sure the ovpIdx -> length 1D entry Id mapping is defined
        require(ovpNeededIndL1DExist.isDefined && ovpNeededIndL1DEntryIdx.isDefined && ovpNeededIndL1DBitvec.isDefined)
        require(ovpNeededIndL1DExist.get.length == numOVP && ovpNeededIndL1DEntryIdx.get.length == numOVP)
        // Get the index of entry that ask this OVP for indirect length 1D
        val needed:   Bool = ovpNeededIndL1DExist.get.apply(ovpIdx)
        val entryIdx: UInt = ovpNeededIndL1DEntryIdx.get.apply(ovpIdx)
        // Needed and selected
        Some(needed && strSelNow(entryIdx) && end1DorDead)
      case None => None
    }

    // State : Idle -> Requested, if this output vector port is needed
    when(ovpsIdle(ovpIdx) && ovpNeeded(ovpIdx)) {
      // Switch to request state
      ovpsReqReg(ovpIdx) := true.B
      // Rewrite the request mask, it is ok to keep the old data until the next request since it will be gate off
      // by not requested
      ovpsMask(ovpIdx) := Mux(
        writeNeeded, // OVP is requested for write
        writeReqMask, // write request mask provided
        Mux( // OVP is not requested for write
          indexNeeded.getOrElse(false.B), // OVP is requested because of indirect index
          indexReqMask.getOrElse(0.U), // indirect index mask provided
          Mux( // OVP is not requested for indirect index
            stride2DNeeded.getOrElse(false.B), // OVP is requested for indirect stride 2D
            stride2DReqMask.getOrElse(0.U), // indirect stride 2D mask is provided
            Mux( // OVP is not requested for indirect stride 2D
              length1DNeeded.getOrElse(false.B), // OVP is requested for indirect Length 1D
              length1DReqMask.getOrElse(0.U), // Indirect Length 1D provided
              0.U // this should not happen, nothing is requested but OVP is needed, makes no sense
            )
          )
        )
      )
    }

      // State : Requested -> Responded, if we get valid from memory write port
      .elsewhen(ovpsRequested(ovpIdx) && memWritePorts(ovpIdx).ovpValid) {
        // Turn off the request, turn on the responded
        ovpsReqReg(ovpIdx) := false.B
        ovpsRspReg(ovpIdx) := true.B
        // Get the write port
        val writePort = memWritePorts(ovpIdx)
        // Rewrite the Vector of TagValue
        ovpsVecData(ovpIdx) := writePort.ovpVecData
        // Rewrite from ovp, they should be kept same
        ovpsMask(ovpIdx) := writePort.ovpValidMask
        // Rewrite the state of stream
        ovpsStreamState match {
          case Some(value) => value(ovpIdx) := writePort.ovpStreamState.getOrElse(0.U.asTypeOf(new StreamState))
          case None        =>
        }
      }

      // State : Responded -> Idle, if this stream entry got selected
      // Please be attention, strSelected is something for both IVP and OVP, so offset needed
      .elsewhen(
        ovpsResponded(ovpIdx) && // First this OVP should have data responded
          // Selected as write (main) stream, or its dependent stream is selected
          (strSelNow(ovpIdx + numIVP) || idxDepSelected.getOrElse(false.B) ||
            s2dDepSelected.getOrElse(false.B) || l1dDepSelected.getOrElse(false.B))
      ) {
        // Turn off the responded, switch to idle
        ovpsRspReg(ovpIdx) := false.B
      }
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Select Entry
  require(selStrEntry.getWidth == selStrWithValues.getWidth)
  selStrEntry := selStrWithValues
  selStrEntry.currStrEntry.origStrEntry.valid := selStrWithValues.currStrEntry.origStrEntry.valid &&
    selStrValid // Check whether this assign overwrite the above

  // Output Vector Port Data Retrieve
  for (ovpIdx <- 0 until numOVP) {
    // Get write port
    val writePort: MemWriteBundle = memWritePorts(ovpIdx)
    // Request means ready
    writePort.memReady := ovpsRequested(ovpIdx)
    // Request Mask
    writePort.memReadyMask := ovpsReadyMask(ovpIdx)
    // The valid of memory type (never change during the lifetime of stream), so set when stream dispatched
    val dispAsMain:     Bool = newStrEntry.valid && strDispatched(numIVP + ovpIdx)
    val dispAsIndirect: Bool = newStrEntry.valid && newStrEntry.LinOrInd.getOrElse(memNode.supportIndirect.B)
    val dispAsIdx: Bool = dispAsIndirect &&
      (newStrEntry.indirectIdxStream.getOrElse(false.B) && newStrEntry.indexOVPortId.getOrElse(0.U) === ovpIdx.U)
    val dispAsL1D: Bool = dispAsIndirect &&
      (newStrEntry.indirectL1DStream.getOrElse(memNode.IndirectLength1DStream.B) &&
        newStrEntry.length1DOVPortId.getOrElse(0.U) === ovpIdx.U)
    val dispAsS2D: Bool = dispAsIndirect &&
      (newStrEntry.indirectS2DStream.getOrElse(memNode.IndirectStride2DStream.B) &&
        newStrEntry.stride2DOVPortId.getOrElse(0.U) === ovpIdx.U)
    val dispToThisOVP: Bool = newStrEntry.valid && (
      // Dispatch as main stream, index stream, length 1d stream, stride 2d stream
      strDispatched(ovpIdx + numIVP) || dispAsIdx || dispAsL1D || dispAsS2D
    )
    // Needed by memory
    writePort.usedByMem := ovpNeeded(ovpIdx) || dispToThisOVP
    writePort.newMemDataType := dispToThisOVP
    // The actual memory type for each case of stream
    writePort.memDataType match {
      case Some(value) =>
        value :=
          // Main stream
          Mux(
            dispAsMain,
            newStrEntry.memDataTypeExp.getOrElse(0.U),
            // Index stream
            Mux(
              dispAsIdx,
              newStrEntry.idxStrDataType.getOrElse(0.U),
              // Length 1D stream
              Mux(
                dispAsL1D,
                newStrEntry.l1dStrDataType.getOrElse(0.U),
                // Stride 2D stream
                Mux(dispAsS2D, newStrEntry.s2dStrDataType.getOrElse(0.U), 0.U)
              )
            )
          )
      case None =>
    }
    // Updated from AGU
    val updAsMain: Bool = updStrEntry.origStrEntry.valid && strSelNow(numIVP + ovpIdx) && updStrEntry.StartOf1D
    val updAsIndirect: Bool = updStrEntry.origStrEntry.valid && updStrEntry.StartOf1D &&
      updStrEntry.origStrEntry.LinOrInd.getOrElse(memNode.supportIndirect.B)
    val updAsIdx: Bool = updAsIndirect &&
      updStrEntry.origStrEntry.indexOVPortId.getOrElse(0.U) === ovpIdx.U &&
      updStrEntry.origStrEntry.indirectIdxStream.getOrElse(memNode.IndirectIndexStream.B)
    val updAsL1D: Bool = updAsIndirect &&
      updStrEntry.origStrEntry.length1DOVPortId.getOrElse(0.U) === ovpIdx.U &&
      updStrEntry.origStrEntry.indirectL1DStream.getOrElse(memNode.IndirectLength1DStream.B)
    val updAsS2D: Bool = updAsIndirect &&
      updStrEntry.origStrEntry.stride2DOVPortId.getOrElse(0.U) === ovpIdx.U &&
      updStrEntry.origStrEntry.indirectS2DStream.getOrElse(memNode.IndirectStride2DStream.B)
    // The update of Length 1D
    val updLength1D: Bool = (dispAsMain || updAsMain) || (dispAsIdx || updAsIdx) ||
      (dispAsL1D || updAsL1D) || (dispAsS2D || updAsS2D)
    writePort.memLength1D.valid := RegNextN(updLength1D, stage = numUpdOvpL1DStage, List(updLength1D)).last
    val updL1DBits: UInt = Mux( // Dispatch as main stream
      dispAsMain,
      newStrEntry.initLength1D.getOrElse(0.U),
      Mux( // Update as main stream
        updAsMain,
        updStrEntry.currLength1Din1D.getOrElse(updStrEntry.origStrEntry.initLength1D.getOrElse(0.U)),
        Mux( // Dispatch as index stream
          dispAsIdx,
          newStrEntry.initLength1D.getOrElse(0.U),
          Mux( // Update as index stream
            updAsIdx,
            updStrEntry.currLength1Din1D.getOrElse(updStrEntry.origStrEntry.initLength1D.getOrElse(0.U)),
            // Indirect L1D/S2D, length1d for these two cases will be one
            Mux(updLength1D, 1.U, 0.U)
          )
        )
      )
    )
    writePort.memLength1D.bits := RegNextN(updL1DBits, stage = numUpdOvpL1DStage, List(updL1DBits)).last
  }

  // Input Vector Port Ignore, since response to input vector port is handle by stream read bus
  memReadPorts.zipWithIndex.foreach { case (ivpPort, ivpIdx) =>
    ivpPort.memValid := DontCare
    ivpPort.memValidMask := DontCare
    ivpPort.memData := DontCare
    if (memNode.nodeType != RecurrenceEngine) {
      ivpPort.usedByMem := strValids(ivpIdx) || outerStrAct.getOrElse(0.U(numIVP.W)).asBools().apply(ivpIdx)
    } else {
      // If Stream Table belongs to recurrence engine, we should check OVP part of stream table
      require(ovpStrTable.nonEmpty)
      ivpPort.usedByMem := VecInit(ovpStrTable.zip(ovpStrValids).map { case (ovpEntry, ovpStrValid) =>
        ovpStrValid && ovpEntry.origStrEntry.recIVPortId.getOrElse(0.U) === ivpIdx.U
      }).asUInt().orR
    }
    ivpPort.memStreamState match {
      case Some(value) => value := DontCare
      case None        =>
    }
    ivpPort.memPadding match {
      case Some(value) => value := DontCare
      case None        =>
    }
    if (ivpPort.broadcastReset.isDefined) ivpPort.broadcastReset.get := DontCare
  }

  // Report memory status to stream dispatcher
  memStatus.alive := atLeastOneStrAlive
  memStatus.newStr := newStrEntry.valid
  memStatus.aguReq := selStrEntry.currStrEntry.origStrEntry.valid && atLeastOneStrAlive
  memStatus.readPause := readPause.getOrElse(false.B) && atLeastOneStrAlive
  memStatus.writePause := writePause.getOrElse(false.B) && atLeastOneStrAlive
  memStatus.portPause := atLeastOneStrAlive && VecInit(strStopByPort).asUInt().andR() && atLeastOneStrAlive
  memStatus.memType := memNode.hwMemType
  memStatus.doingAtomOp5 := VecInit(strMemOp.map(_.apply(7))).asUInt().orR()
  memStatus.doingAtomOp4 := VecInit(strMemOp.map(_.apply(6))).asUInt().orR()
  memStatus.doingAtomOp3 := VecInit(strMemOp.map(_.apply(5))).asUInt().orR()
  memStatus.doingAtomOp2 := VecInit(strMemOp.map(_.apply(4))).asUInt().orR()
  memStatus.doingAtomOp1 := VecInit(strMemOp.map(_.apply(3))).asUInt().orR()
  memStatus.doingAtomOp0 := VecInit(strMemOp.map(_.apply(2))).asUInt().orR()
  memStatus.doingWrite := VecInit(strMemOp.map(_.apply(1))).asUInt().orR()
  memStatus.doingRead := VecInit(strMemOp.map(_.apply(0))).asUInt().orR()

  // Report Stream Valid
  strVlds := strValids.asUInt()

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  /* ------------------------- Utility                      ------------------------- */

  // Indirect Value connection from ovp
  def indirectConnectionFromOVP[T <: Data](
    entryIndValue:    Option[Vec[UInt]],
    entryIndValid:    Option[Vec[UInt]],
    entryIndOVPortId: Seq[Option[UInt]],
    connectValid:     Boolean = true
  ): Unit = {
    entryIndValue match {
      case Some(indValues) =>
        // If there is value, there is valid
        require(entryIndValid.isDefined && entryIndOVPortId.forall(_.isDefined))
        require(
          indValues.length == numEntry &&
            entryIndValid.get.length == numEntry && entryIndOVPortId.length == numEntry
        )
        for (entryIdx <- 0 until numEntry) {
          // Indirect OVP Port ID
          val indOVPortId: UInt = entryIndOVPortId(entryIdx).get
          require(indOVPortId.getWidth == log2Ceil(numOVP))
          // Assign to the wire
          indValues(entryIdx) := ovpsData(indOVPortId)
          val validMask:     UInt = ovpsValidMask(indOVPortId)
          val portResponded: Bool = ovpsResponded(indOVPortId)
          // Do not connect for Stride2D and Length 1D, just use ovpResponded
          if (connectValid) entryIndValid.get.apply(entryIdx) := Mux(portResponded, validMask, 0.U)
        }
      case None =>
    }
  }

  // Connect stream entry to stream outstanding entry
  def disp2table(newStrEntry: StreamEntry, outstandStr: StreamEntryOutstanding): Unit = {
    outstandStr.origStrEntry := newStrEntry
    // Outstanding Start Point Tracking
    outstandStr.currStartPoint1D match {
      case Some(value) =>
        require(newStrEntry.startPoint.isDefined)
        value := newStrEntry.startPoint.get
      case None =>
    }
    outstandStr.currStartPoint2D match {
      case Some(value) =>
        require(newStrEntry.startPoint.isDefined)
        value := newStrEntry.startPoint.get
      case None =>
    }
    outstandStr.currStartPoint3D match {
      case Some(value) =>
        require(newStrEntry.startPoint.isDefined)
        value := newStrEntry.startPoint.get
      case None =>
    }
    // Outstanding Length 1D Tracking
    outstandStr.currLength1Din1D match {
      case Some(value) =>
        require(newStrEntry.initLength1D.isDefined)
        value := newStrEntry.initLength1D.get
      case None =>
    }
    outstandStr.currLength1Din2D match {
      case Some(value) =>
        require(newStrEntry.initLength1D.isDefined)
        value := newStrEntry.initLength1D.get
      case None =>
    }
    outstandStr.currLength1Din3D match {
      case Some(value) =>
        require(newStrEntry.initLength1D.isDefined)
        value := newStrEntry.initLength1D.get
      case None =>
    }
    // Outstanding Length 2D tracking
    outstandStr.currLength2Din2D match {
      case Some(value) =>
        require(newStrEntry.initLength2D.isDefined)
        value := newStrEntry.initLength2D.get
      case None =>
    }
    outstandStr.currLength2Din3D match {
      case Some(value) =>
        require(newStrEntry.initLength2D.isDefined)
        value := newStrEntry.initLength2D.get
      case None =>
    }
    // Outstanding Length 3D tracking
    outstandStr.currLength3D match {
      case Some(value) =>
        require(newStrEntry.initLength3D.isDefined)
        value := newStrEntry.initLength3D.get
      case None =>
    }
    // Outstanding Stride2D and Stretch2D Tracking
    outstandStr.currStride2D match {
      case Some(value) =>
        require(newStrEntry.stride2D.isDefined)
        value := newStrEntry.stride2D.get
      case None =>
    }
    outstandStr.currStretch2D match {
      case Some(value) =>
        require(newStrEntry.stretch2D.isDefined)
        value := newStrEntry.stretch2D.get
      case None =>
    }
    // Assign the state of stream, since it is new, so we know it is start for everything
    outstandStr.StartOfStream := true.B
    outstandStr.StartOf1D := true.B
    outstandStr.StartOf2D := true.B
  }

  /* ------------------------- Post Generation Sanity Check ------------------------- */

  // The vector length of output vector port should be the same, equal to bandwidth
  memWritePorts.zipWithIndex.foreach { case (writePort, i) =>
    require(
      writePort.ovpVecData.length == memNode.bandwidth,
      s"The $i write port's width is not equal to memory bandwidth"
    )
  }
}

object StreamTable {

  /** Round Robin Stream Issue for stream entry with its dependent values
    *
    * @param memNode            Memory Node Parameters
    * @param strValid4Selection Stream is Ready for selection
    * @param outReady           The downstream is ready to take a new stream issue
    * @return (Valid of Selected Stream Entry,
    *         Selected Signal for each Stream Entry,
    *         hardware index pointed to the stream entry)
    */
  def RRStreamSelection(
    memNode:            MemNodeParameters,
    strValid4Selection: Seq[Bool],
    outReady:           Bool
  )(
    implicit p: Parameters
  ): (Bool, Seq[Bool], UInt) = {
    // Extract parameters
    val numStr: Int = strValid4Selection.length
    // Round Robin Arbiter is used
    val arb: RRArbiter[UInt] = Module(new RRArbiter[UInt](UInt(1.W), numStr))
    // Connect to arbiter
    arb.io.in.zip(strValid4Selection).foreach { case (port, valid) => port.bits := false.B; port.valid := valid }
    // Connect the output ready
    arb.io.out.ready := outReady
    // Return
    (arb.io.out.valid, arb.io.in.map(_.ready), arb.io.chosen)
  }
}
