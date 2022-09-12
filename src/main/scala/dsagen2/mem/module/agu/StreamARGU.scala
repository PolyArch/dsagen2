package dsagen2.mem.module.agu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.mem.bundle.{StreamEntry, StreamEntryOutstanding, StreamEntryWithValues, StreamRequest}
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.misc.module.MaskDistributor
import dsagen2.top.config.DSAFixedConfig.memOpRead
import dsagen2.util.StreamUtil._
import dsagen2.util.UIntUtil.{allOneUInt, groupBitsAs}
import freechips.rocketchip.tile.XLen

/** Stream Address Request Generation Unit
  *
  * Input:
  *  - Force kill the liveness of updated stream
  *  - Select stream entry for address generation
  *    Output:
  *  - Updated stream entry that is going to be written in to stream table
  *  - Vector of stream request port that carries stream request (address + mask + meta info)
  *
  * @param memNode   Memory Node Parameters
  * @param ivpsParam IVP Parameters
  * @param ovpsParam OVP Parameters
  * @param p         CDE
  */
class StreamARGU(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit val p: Parameters)
    extends MultiIOModule {

  /* -------------------------      Extract Parameters      ------------------------- */

  /* -------------------------     Derived Parameters       ------------------------- */

  def numIVP: Int = ivpsParam.length

  def numOVP: Int = ovpsParam.length

  /* -------------------------      Parameters Sanity Check ------------------------- */

  /* -------------------------         Input / Output       ------------------------- */

  // Force kill stream input signal
  val forceKill: Bool = IO(Input(Bool()))

  // Select Stream Entry From Stream Table
  val selStrEntry: StreamEntryWithValues = IO(Input(new StreamEntryWithValues(memNode, ivpsParam, ovpsParam)))

  // Updated Stream Entry send back up stream table
  val updStrEntry: StreamEntryOutstanding = IO(Output(new StreamEntryOutstanding(memNode, ivpsParam, ovpsParam)))

  // Generated Memory Request sent to Memory Request Interface
  val strReqPorts: Vec[StreamRequest] = IO(
    Output(Vec(memNode.numMemReqPort, new StreamRequest(memNode, ivpsParam, ovpsParam)))
  )

  /* -------------------------     Registers and Modules    ------------------------- */

  /* -------------------------             Wires            ------------------------- */

  // Current selected stream entry
  val currStrEntry: StreamEntryOutstanding = selStrEntry.currStrEntry

  // The original stream entry when dispatched
  val origStrEntry: StreamEntry = currStrEntry.origStrEntry

  // Wire that indicate whether the Linear 1D Stream is finished
  // Not exist means ending cannot be determined by this, so getOrElse should be false.B
  val linear1DLast: Option[Bool] = if (memNode.Linear1DEnabled) Some(WireInit(false.B)) else None

  // Wire that indicate whether the Linear 2D Stream is finished
  // Not exist means ending cannot be determined by this, so getOrElse should be false.B
  val linear2DLast: Option[Bool] = if (memNode.Linear2DEnabled) Some(WireInit(false.B)) else None

  // Wire that indicate whether the Linear 3D Stream is finished
  // Not exist means ending cannot be determined by this, so getOrElse should be false.B
  val linear3DLast: Option[Bool] = if (memNode.Linear3DEnabled) Some(WireInit(false.B)) else None

  // Length Zero Check
  val zeroLength: Bool = WireInit(false.B)

  // Wire that indicate whether the linear pattern is finished
  // TODO: purely indirect may never end, we need to use index value to specify the end of stream,
  //  which will set the forceKill signal
  val linearEnd: Bool = linear1DLast.getOrElse(false.B) &&
    linear2DLast.getOrElse(true.B) &&
    linear3DLast.getOrElse(true.B)

  // Wire that indicate whether the current stream finished
  val updStrAlive: Bool = !forceKill && !linearEnd && origStrEntry.valid && !zeroLength

  // The number of accessed elements of this DataType Linear in 1D stream, value: 0 ... memWidthByte
  val numAcceL1DElemWire: Option[UInt] =
    if (memNode.needAGU) Some(WireInit(0.U(log2Ceil(memNode.bandwidth + 1).W))) else None

  // The number of finished linear 1D stream in this request
  // TODO: we should make the AGU be able to finish multiple 1D stream request, this will be equal to the number of
  //  finished 1D stream
  val numFinish1DStrWire: Option[UInt] = if (memNode.Linear2DEnabled) {
    // TODO: for now, AGU can only complete up to one 1D stream at a time
    Some(Mux(linear1DLast.getOrElse(false.B), 1.U(1.W), 0.U(1.W)))
  } else None

  // The number of finished linear 2D stream in this request
  // TODO: we CAN make the AGU be able to finish multiple 2D stream request??? this will be equal to the number of
  //  finished 2D stream. Really? How hard it is? I don't know, sounds super expensive
  val numFinish2DStrWire: Option[UInt] = if (memNode.Linear3DEnabled) {
    // TODO: for now, AGU can only complete up to one 2D stream at a time
    Some(Mux(linear1DLast.getOrElse(false.B) && linear2DLast.getOrElse(false.B), 1.U(1.W), 0.U(1.W)))
  } else None

  // The linear bitmask of accessed elements, UInt whose width is memWidthByte
  val linearBitmask: Option[UInt] =
    if (memNode.needAGU) Some(WireInit(0.U(memNode.bandwidth.W))) else None

  // The linear address (I believe this should be identical with currStartPoint1D for linear stream)
  val linearAddr: Option[UInt] = if (memNode.needAddr) Some(WireInit(0.U(memNode.addrBits.W))) else None

  // The indirect bitmask of accessed element, width of Wire is memWidthByte
  val indirectBitmask: Option[Vec[UInt]] =
    if (memNode.IndirectIndexStream) {
      if (memNode.isDMA || memNode.isGEN) {
        Some(WireInit(VecInit(Seq.fill(1)(0.U(memNode.bandwidth.W)))))
      } else if (memNode.isSPM) {
        Some(WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(0.U(memNode.spmBankWidth.W)))))
      } else None
    } else None

  // The indirect addresses
  val indirectAddrs: Option[Vec[UInt]] =
    if (memNode.IndirectIndexStream && memNode.needAddr) {
      if (memNode.isDMA || memNode.isGEN) {
        Some(WireInit(VecInit(Seq.fill(1)(0.U(memNode.addrBits.W)))))
      } else if (memNode.isSPM) {
        Some(WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(0.U(memNode.addrBits.W)))))
      } else None
    } else None

  // The valid of each indirect address
  val indirectAddrValids: Option[Vec[Bool]] =
    if (memNode.IndirectIndexStream && memNode.needAddr) {
      if (memNode.isDMA || memNode.isGEN) {
        Some(WireInit(VecInit(Seq.fill(1)(false.B))))
      } else if (memNode.isSPM) {
        Some(WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(false.B))))
      } else None
    } else None

  // The number of accessed elements of this DataType Linear in 1D stream, value: 0 ... numReqPort
  val numAcceI1DElemWire: Option[UInt] =
    if (memNode.supportIndirect && memNode.IndirectIndexStream)
      Some(WireInit(0.U(log2Ceil(memNode.numMemReqPort + 1).W)))
    else None

  // Request Addresses that merging linear address and indirect address
  val reqAddresses: Option[Vec[UInt]] =
    if (memNode.needAddr) Some(WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(0.U(memNode.addrBits.W)))))
    else None

  // Request Mask that merging linear and indirect mask
  val reqBitmasks: Option[Vec[UInt]] =
    if (memNode.hasStrRequest) {
      Some(
        WireInit(
          VecInit(
            Seq.fill(memNode.numMemReqPort)(
              0.U(
                if (memNode.isDMA || memNode.isGEN || memNode.isREC) {
                  memNode.bandwidth.W
                } else if (memNode.isSPM) {
                  memNode.spmBankWidth.W
                } else {
                  require(requirement = false)
                  1.W
                }
              )
            )
          )
        ).suggestName("reqBitmasks")
      )
    } else None

  // Wire Vector that holds the data that going to be written in to memory's each request port, distributed by mask
  val reqWriteDataWire: Option[Vec[UInt]] =
    if (memNode.canDoWrite) {
      // For write data wire, DMA/REC/DIS is equal to band bit width
      if (memNode.isDMA || memNode.isREC || memNode.isDIS) {
        Some(WireInit(VecInit(Seq.fill(1)(0.U(memNode.bandBits.W)))))
      } else if (memNode.isSPM) {
        // Scratchpad write data wire = #bank x bankwidth
        Some(WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(0.U(memNode.spmBankBitWidth.W)))))
      } else if (memNode.isREG) {
        // Register engine = 1 x XLEN-bit
        Some(WireInit(VecInit(Seq.fill(1)(0.U(p(XLen).W)))))
      } else {
        require(requirement = false, s"Write Data Wire needed for $memNode?");
        None
      }
    } else None

  // Wire that holds the updated updated current start point of linear 1D if linear 1D stream continues
  val updStartPoint1D: Option[UInt] =
    if (selStrEntry.currStrEntry.currStartPoint1D.isDefined)
      Some(WireInit(0.U(selStrEntry.currStrEntry.currStartPoint1D.get.getWidth.W)))
    else None

  // Wire that holds the updated updated current start point of linear 2D if linear 2D stream continues
  val updStartPoint2D: Option[UInt] =
    if (selStrEntry.currStrEntry.currStartPoint2D.isDefined)
      Some(WireInit(0.U(selStrEntry.currStrEntry.currStartPoint2D.get.getWidth.W)))
    else None

  // Wire that holds the updated updated current start point of linear 3D if linear 3D stream continues
  val updStartPoint3D: Option[UInt] =
    if (selStrEntry.currStrEntry.currStartPoint3D.isDefined)
      Some(WireInit(0.U(selStrEntry.currStrEntry.currStartPoint3D.get.getWidth.W)))
    else None

  // Wire that holds the updated Length 1D if Linear 1D stream continues
  val updL1Din1D: Option[UInt] = selStrEntry.currStrEntry.currLength1Din1D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  // Wire that holds the updated Length 1D if Linear 2D stream continues
  val updL1Din2D: Option[UInt] = selStrEntry.currStrEntry.currLength1Din2D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  // Wire that holds the updated Length 1D if Linear 3D stream continues
  val updL1Din3D: Option[UInt] = selStrEntry.currStrEntry.currLength1Din3D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  // Wire that holds the updated Length 2D if Linear 2D stream continues
  val updL2Din2D: Option[UInt] = selStrEntry.currStrEntry.currLength2Din2D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  // Wire that holds the updated Length 2D if Linear 3D stream continues
  val updL2Din3D: Option[UInt] = selStrEntry.currStrEntry.currLength2Din3D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  // Wire that holds the updated Length 3D if Linear 3D stream continues
  val updL3D: Option[UInt] = selStrEntry.currStrEntry.currLength3D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  // Wire that holds the updated Stride 2D if Linear 3D stream continues
  val updStride2D: Option[UInt] = selStrEntry.currStrEntry.currStride2D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  // Wire that holds the updated Stretch 2D if Linear 3D stream continues
  val updStretch2D: Option[UInt] = selStrEntry.currStrEntry.currStretch2D match {
    case Some(value) => Some(WireInit(0.U(value.getWidth.W)))
    case None        => None
  }

  /* -------------------------     Combinational Logics     ------------------------- */

  // Calculate the correct length 1D by taking the indirect length1D into account
  // we should use indirect length 1D if it is running at indirect L1D stream mode and at the beginning of 1D stream
  val actualL1D: UInt = Mux(
    currStrEntry.origStrEntry.indirectL1DStream.getOrElse(memNode.IndirectLength1DStream.B) &&
      currStrEntry.StartOf1D,
    // If Stream is running at indirect l1d mode and it is the start of 1d stream, then we should use indirect L1D
    // as actual L1D
    selStrEntry.indL1DValue.getOrElse(0.U),
    // otherwise the current Length 1D in 1D stream should works
    currStrEntry.currLength1Din1D.getOrElse(currStrEntry.origStrEntry.initLength1D.getOrElse(0.U))
  )

  // Calculate whether linear 1D stream is finished
  linear1DLast match {
    case Some(value) =>
      require(numAcceL1DElemWire.isDefined)
      value := forceKill || actualL1D <= numAcceL1DElemWire.get
    case None =>
  }

  // Calculate whether linear 2D stream is finished
  linear2DLast match {
    case Some(value) =>
      require(numFinish1DStrWire.isDefined)
      value := forceKill ||
        Mux(
          origStrEntry.numLinDim.getOrElse(0.U) > 0.U,
          currStrEntry.currLength2Din2D.getOrElse(0.U) <= numFinish1DStrWire.get,
          true.B /*If dimension is not enough, it is the last*/
        )
    case None =>
  }

  // Calculate whether linear 3D stream is finished
  linear3DLast match {
    case Some(value) =>
      require(numFinish2DStrWire.isDefined)
      value := forceKill ||
        Mux(
          origStrEntry.numLinDim.getOrElse(0.U) > 1.U,
          currStrEntry.currLength3D.getOrElse(0.U) <= numFinish2DStrWire.get,
          true.B /*If dimension is not enough, it is the last*/
        )
    case None =>
  }

  // Check whether the length of selected stream is zero
  // Either Length 1D is zero, or Length 2D is zero (2D stream), or Length 3D is zero (3D stream)
  zeroLength := actualL1D === 0.U ||
    Mux(origStrEntry.numLinDim.getOrElse(0.U) > 0.U, currStrEntry.currLength2Din2D.getOrElse(1.U) === 0.U, false.B) ||
    Mux(origStrEntry.numLinDim.getOrElse(0.U) > 1.U, currStrEntry.currLength3D.getOrElse(1.U) === 0.U, false.B)

  // Calculate number of 1D Element accessed and linear access bitmask
  if (memNode.needAGU) {
    require(numAcceL1DElemWire.isDefined && linearBitmask.isDefined)
    // Select the correct Length 1D
    // If it is DMA and running at indirect index stream, then as long as the above length 1D is not zero, then
    // the actual length 1D should be 1.U
    val indDMAL1D: UInt = Mux(
      memNode.isDMA.B && origStrEntry.indirectIdxStream.getOrElse(memNode.IndirectIndexStream.B),
      1.U(actualL1D.getWidth.W),
      actualL1D
    )

    // Get AGU result
    val (bitmask, numAccess) = LinearStreamAGU.LinearMaskGen(
      linearAddr.getOrElse( // Linear Address with indirect stride 2d combined
        currStrEntry.currStartPoint1D.getOrElse( // just linear address
          currStrEntry.origStrEntry.startPoint.getOrElse( // original start point
            0.U(log2Ceil(memNode.bandwidth).W) // zero
          )
        )
      ),
      indDMAL1D,
      origStrEntry.memDataTypeExp,
      origStrEntry.stride1D,
      memNode
    )
    // Connect to wire
    require(linearBitmask.get.getWidth == bitmask.getWidth)
    linearBitmask.get := bitmask
    require(numAcceL1DElemWire.get.getWidth == numAccess.getWidth)
    numAcceL1DElemWire.get := numAccess
  } else {
    require(numAcceL1DElemWire.isEmpty && linearBitmask.isEmpty)
  }

  // Connect linear address
  linearAddr match {
    case Some(value) =>
      // Calculate Stride 2D, consider indirect stride 2D
      val s2d: UInt = {
        if (memNode.NumStride2DUnitBitsExp > 0) {
          // Get the value of stride 2d
          val rawS2D: UInt = actualValueOfSignUIntWithDataType(
            value.getWidth,
            selStrEntry.indS2DValue,
            origStrEntry.s2dStrDataType,
            memNode.NumStride2DUnitBitsExp
          )
          // Get the valid bit of indirect s2d
          val s2dValid: Bool = memNode.IndirectStride2DStream.B && selStrEntry.indS2DValid.getOrElse(false.B) &&
            origStrEntry.LinOrInd.getOrElse(true.B) && origStrEntry.indirectS2DStream.getOrElse(true.B)
          // Combine with stride 2d
          val validIndS2D: UInt = Mux(s2dValid, rawS2D, 0.U)
          // Shift by memory type
          (validIndS2D << origStrEntry.memDataTypeExp.getOrElse(0.U)).asUInt()
        } else 0.U
      }
      // Add current start point with stride 2D
      value := currStrEntry.currStartPoint1D.getOrElse(0.U) + s2d
    case None =>
  }

  // Calculate indirect address, bitmask, valid and number of accessed Indirect 1D element
  indirectBitmask match {
    case Some(indMasks) =>
      require(indirectAddrs.isDefined)
      require(indirectAddrValids.isDefined)
      require(numAcceI1DElemWire.isDefined)
      val indAddrs:         Vec[UInt] = indirectAddrs.get
      val indAddrValids:    Vec[Bool] = indirectAddrValids.get
      val numAccessI1DElem: UInt = numAcceI1DElemWire.get
      require(indMasks.length == indAddrs.length)
      require(indAddrValids.length == indAddrs.length)
      require(selStrEntry.indIdxValues.isDefined)
      require(selStrEntry.indIdxValues.isDefined)
      // Indirect Address and Bitmask generation
      val (iAddrs, iMasks, iValids, numAccess) =
        IndirectStreamAGU.IndirectAddrGen(
          currStrEntry.currStartPoint1D.get,
          origStrEntry.stride1D,
          origStrEntry.memDataTypeExp,
          selStrEntry.indIdxValues.get,
          selStrEntry.indIdxValids.get,
          origStrEntry.idxStrDataType,
          memNode
        )
      // Connect generated results
      // width check is turned off since generated address will be extended by indirect data type
      vecConnect(indAddrs, iAddrs, widthCheck = false)
      vecConnect(indAddrValids, iValids)
      vecConnect(indMasks, iMasks)
      numAccessI1DElem := numAccess
    case None =>
      require(indirectAddrs.isEmpty)
      require(indirectAddrValids.isEmpty)
      require(numAcceI1DElemWire.isEmpty)
  }

  // Calculate updated start point of linear 1D if 1D stream continues
  updStartPoint1D match {
    case Some(value) =>
      value := {
        // Get the current one
        val curr: UInt = currStrEntry.currStartPoint1D.getOrElse(origStrEntry.startPoint.getOrElse(0.U))

        // Constant stream (always supported) will be itself
        val constNext: UInt = curr

        // Ascending result (stride1D == 1) will be start address of next memory line
        val ascend1Next: Option[UInt] = if (memNode.MaxAbsStride1D > 0) {
          // Get the line address from the curr higher bits
          val highCurr: UInt = curr(curr.getWidth - 1, log2Ceil(memNode.bandwidth))
          // next line address is higher part increased by 1 and lower are all zero
          val res: UInt = Cat(highCurr + 1.U(1.W), Fill(log2Ceil(memNode.bandwidth), 0.U(1.W)))
          // Check width
          require(
            res.getWidth == curr.getWidth,
            s"Next ascending line address is ${res.getWidth}-bit, " +
              s"but the current address is ${curr.getWidth}-bit"
          )
          // Return
          Some(res)
        } else None

        // Descending result by 1 (stride1D == -1) will be end address of previous memory line
        val descend1Next: Option[UInt] = if (memNode.MaxAbsStride1D > 1) {
          // Get the line address from the curr higher bits
          val highCurr: UInt = curr(curr.getWidth - 1, log2Ceil(memNode.bandwidth))
          // next line address is higher part decreased by 1 and lower are all one
          val res: UInt = Cat(highCurr - 1.U(1.W), Fill(log2Ceil(memNode.bandwidth), 1.U(1.W)))
          // Check width
          require(
            res.getWidth == curr.getWidth,
            s"Next descending line address is ${res.getWidth}-bit, " +
              s"but the current address is ${curr.getWidth}-bit"
          )
          // Return
          Some(res)
        } else None

        // Descending or Ascending by n (stride1D != -1/0/1), calculated by multiplication
        val generalNext: Option[UInt] = if (memNode.MaxAbsStride1D > 0) {
          // Stride1D must exist
          require(origStrEntry.stride1D.isDefined)
          require(origStrEntry.stride1D.get.getWidth > 0)
          val s1D: SInt = origStrEntry.stride1D.get.asSInt()
          // Next = curr + #access * stride1D
          val diff: UInt = {
            if (memNode.MaxAbsStride1D > 2) (s1D * numAcceL1DElemWire.getOrElse(0.U)).asUInt()
            else Mux(origStrEntry.stride1D.get === 1.U, numAcceL1DElemWire.getOrElse(0.U), 0.U)
          }
          val next: UInt = curr + (diff << origStrEntry.memDataTypeExp.getOrElse(0.U)).asUInt()
          // Return
          Some(next(curr.getWidth - 1, 0))
        } else None

        // Get result and check width
        val result: UInt = origStrEntry.stride1D match {
          case Some(s1d) => // MuxLookup(s1d, curr, nextLUT)
            Mux(
              s1d === 0.U,
              constNext, // Constant Stream
              Mux(
                // Continuous ascending stream, since indirect stride 2d will change curr, so switch to general
                s1d === 1.U && !origStrEntry.indirectS2DStream.getOrElse(false.B),
                ascend1Next.getOrElse(curr), // Ascending Stream
                Mux(
                  // Continuous descending stream, since indirect stride 2d will change curr, so switch to general
                  s1d === -1.S(origStrEntry.stride1D.get.getWidth.W).asUInt() &&
                    !origStrEntry.indirectS2DStream.getOrElse(false.B),
                  descend1Next.getOrElse(curr), // Descending Stream
                  generalNext.getOrElse(curr) // General Stride1D Stream
                )
              )
            )
          case None => curr // constant
        }

        require(result.getWidth <= value.getWidth, s"result'bit = ${result.getWidth}, value'bit = ${value.getWidth}")
        // Return
        result
      }
    case None =>
  }

  // Calculate updated start point of linear 2D stream if 2D stream continues
  updStartPoint2D match {
    case Some(next) =>
      require(currStrEntry.currStartPoint2D.isDefined)
      val curr: UInt = currStrEntry.currStartPoint2D.get
      require(next.getWidth == curr.getWidth)
      require(
        memNode.IndirectStride2DStream || memNode.MaxAbsStride2D > 0,
        s"If none stride2d pattern supported, why you have Start Point of 2D"
      )
      // Calculate the linear stride 2d
      val linDelta: UInt =
        currStrEntry.currStride2D match {
          case Some(value) => actualValueOfSignUInt(curr.getWidth, Some(value))
          case None        => actualValueOfSignUInt(curr.getWidth, origStrEntry.stride2D)
        }
      // Assign, left shift final delta combined with memory data type
      next := curr + (linDelta << origStrEntry.memDataTypeExp.getOrElse(0.U))
    case None =>
  }

  // Calculate updated start point of linear 3D stream if 3D stream continues
  updStartPoint3D match {
    case Some(next) =>
      require(currStrEntry.currStartPoint3D.isDefined)
      val curr: UInt = currStrEntry.currStartPoint3D.get
      // Calculate the linear stride 3D
      val linDelta: UInt = actualValueOfSignUInt(curr.getWidth, origStrEntry.stride3D)
      // Assign
      next := curr + (linDelta << origStrEntry.memDataTypeExp.getOrElse(0.U))
    case None =>
  }

  // Calculate updated Length 1D in 1D if 1D Stream continues, combining the linear and indirect # access
  updL1Din1D match {
    case Some(next) =>
      val curr: UInt = actualL1D
      // Calculate the delta
      val delta: UInt = WireInit(0.U(curr.getWidth.W))
      if (origStrEntry.LinOrInd.isEmpty) {
        if (memNode.supportLinear) {
          delta := numAcceL1DElemWire.get // Only linear
        } else if (memNode.IndirectIndexStream) {
          delta := numAcceI1DElemWire.get // Only indirect
        } else {
          require(requirement = false, s"Memory node with no pattern?")
        }
      } else {
        // Combine both linear and indirect access number
        require(numAcceL1DElemWire.isDefined && numAcceI1DElemWire.isDefined)
        // Indirect number of accessed element can only be used when
        delta := Mux(origStrEntry.indirectIdxStream.getOrElse(false.B), numAcceI1DElemWire.get, numAcceL1DElemWire.get)
      }
      next := curr - delta // just decrease by the number of accessed
    case None =>
  }

  // Combining linear and indirect requesting addresses, combining linear and indirect
  reqAddresses match {
    case Some(reqAddrs) =>
      if (origStrEntry.LinOrInd.isEmpty) {
        if (memNode.supportLinear) {
          require(linearAddr.isDefined)
          reqAddrs.zipWithIndex.foreach { case (addr, idx) =>
            if (memNode.isSPM && memNode.numSpmBank > 1) {
              val bankAddr: UInt = lineAddr2bankAddr(linearAddr.get, idx, memNode.numSpmBank, memNode.spmBankWidth)
              require(bankAddr.getWidth == linearAddr.get.getWidth)
              addr := bankAddr
            } else {
              addr := linearAddr.get
            }
          }
        } else if (memNode.IndirectIndexStream) {
          require(indirectAddrs.isDefined)
          require(reqAddrs.length == indirectAddrs.get.length)
          reqAddrs.zip(indirectAddrs.get).foreach { case (k, s) => k := s }
        } else {
          require(requirement = false, s"Memory node with no pattern?")
        }
      } else {
        require(linearAddr.isDefined && indirectAddrs.isDefined)
        require(reqAddrs.length == indirectAddrs.get.length)
        reqAddrs.zipWithIndex.foreach { case (rAddr, idx) =>
          val linAddr: UInt = {
            if (memNode.isSPM && memNode.numSpmBank > 1) {
              val bankAddr: UInt = lineAddr2bankAddr(linearAddr.get, idx, memNode.numSpmBank, memNode.spmBankWidth)
              bankAddr.suggestName(s"bank${idx}_addr")
            } else {
              linearAddr.get
            }
          }
          rAddr := Mux(origStrEntry.indirectIdxStream.getOrElse(false.B), indirectAddrs.get.apply(idx), linAddr)
        }
      }
    case None =>
  }

  // Combining linear and indirect bitmask
  reqBitmasks match {
    case Some(reqMasks) =>
      // Group Write Data Mask
      val groupWriteValid: Seq[UInt] =
        if (memNode.canDoWrite) groupBitsAs(selStrEntry.writeValids.get.asUInt(), reqMasks.head.getWidth)
        else Seq.fill(reqMasks.length)(allOneUInt(reqMasks.head.getWidth))
      // If only one stream pattern exist
      if (origStrEntry.LinOrInd.isEmpty) {
        // Only support linear pattern
        if (memNode.supportLinear) {
          require(linearBitmask.isDefined)
          require(reqMasks.getWidth == linearBitmask.get.getWidth)
          // Group the mask from linear generated
          val groupMask: Seq[UInt] = groupBitsAs(linearBitmask.get, reqMasks.head.getWidth)
          require(reqMasks.length == groupWriteValid.length)
          reqMasks.zip(groupMask).zip(groupWriteValid).foreach { case ((k, s), v) =>
            require(k.getWidth == s.getWidth && k.getWidth == v.getWidth)
            // If memory request is read, then ignore predication, if it is write,
            // then turn off by predication and write data valid
            // TODO: please fix the following three places--disabled by predication.
            //  predication cannot be combined with bitmask. It can be used at the same place with
            //  valid, but not bitmask
            k := Mux(origStrEntry.memOperation.getOrElse(memOpRead) === memOpRead, s, s /*& p & v*/ )
            k.suggestName("reqBitmaskWithPred")
          }
        } else if (memNode.IndirectIndexStream) {
          // Only support indirect pattern
          require(indirectBitmask.isDefined)
          require(reqMasks.length == indirectBitmask.get.length && reqMasks.length == groupWriteValid.length)
          reqMasks.zip(indirectBitmask.get).zip(groupWriteValid).foreach { case ((sink, source), valid) =>
            require(sink.getWidth == source.getWidth && sink.getWidth == valid.getWidth)
            sink := Mux(
              origStrEntry.memOperation.getOrElse(memOpRead) === memOpRead,
              source,
              source /*& pred & valid*/
            )
          }
        } else {
          require(requirement = false, s"Memory node with no pattern?")
        }
      } else {
        // Both pattern exist
        require(linearBitmask.isDefined)
        require(reqMasks.getWidth == linearBitmask.get.getWidth)
        val linearMasks: Seq[UInt] = groupBitsAs(linearBitmask.get, reqMasks.head.getWidth)
        require(reqMasks.length == linearMasks.length)
        require(reqMasks.length == groupWriteValid.length)
        require(indirectBitmask.isDefined)
        require(reqMasks.length == indirectBitmask.get.length)
        reqMasks.zipWithIndex.zip(groupWriteValid).foreach { case ((sink, idx), valid) =>
          val linMask: UInt = linearMasks(idx)
          val indMask: UInt = indirectBitmask.get.apply(idx)
          require(sink.getWidth == linMask.getWidth)
          require(sink.getWidth == indMask.getWidth)
          require(sink.getWidth == valid.getWidth)
          // Get generated mask
          sink := Mux(origStrEntry.indirectIdxStream.getOrElse(false.B), indMask, linMask)
        }
      }
    case None =>
  }

  // Connect to write Data from selected entry
  reqWriteDataWire match {
    case Some(reqDataVec) =>
      if (!memNode.isDIS) {
        require(selStrEntry.writeVecData.isDefined && selStrEntry.writeValids.isDefined)
        require(reqBitmasks.isDefined)
        val writeDataValueVec: Vec[UInt] = selStrEntry.writeData
        require(reqDataVec.getWidth == writeDataValueVec.getWidth)
        require(reqBitmasks.get.getWidth == reqDataVec.getWidth / memNode.memUnitBits)
        require(reqBitmasks.get.getWidth == writeDataValueVec.length)
        // Distributor is needed since data is aggregated in lower part
        val disData: Vec[UInt] =
          VecInit(MaskDistributor(writeDataValueVec, VecInit(reqBitmasks.get.asUInt().asBools()))._1)
        val disDataGroupUInt: Seq[UInt] = groupBitsAs(disData.asUInt(), reqDataVec.head.getWidth)
        require(reqDataVec.length == disDataGroupUInt.length)
        reqDataVec.zip(disDataGroupUInt).foreach { case (int, int1) =>
          require(int.getWidth == int1.getWidth)
          int := int1
        }
      }
    case None =>
  }

  // Calculate updated Length 1D in 2D if 2D stream continues (combine linear and indirect)
  updL1Din2D match {
    case Some(next) =>
      require(currStrEntry.currLength1Din2D.isDefined)
      val curr: UInt = currStrEntry.currLength1Din2D.get
      require(next.getWidth == curr.getWidth)
      require(
        memNode.IndirectLength1DStream || memNode.MaxAbsStretch2D > 0,
        s"If none stride2d pattern supported, why you have Length1D of 2D"
      )
      // Calculate the indirect length 1d in 2d, this is not the delta but actual value
      val indValue: UInt = {
        // Create same width wire
        val sameWidthUInt = WireInit(0.U(curr.getWidth.W))
        selStrEntry.indL1DValue match {
          // This is just make the width the same
          case Some(value) => sameWidthUInt := value
          case None        =>
        }
        // return
        sameWidthUInt
      }
      // Calculate the linear stretch 2d, this is delta not actual value
      val linDelta: UInt = {
        currStrEntry.currStretch2D match {
          case Some(value) => actualValueOfSignUInt(curr.getWidth, Some(value))
          case None        => actualValueOfSignUInt(curr.getWidth, origStrEntry.stretch2D)
        }
      }
      // Assign
      next := Mux(
        memNode.IndirectIndexStream.B && selStrEntry.indL1DValid.getOrElse(false.B) &&
          origStrEntry.LinOrInd.getOrElse(true.B) && origStrEntry.indirectL1DStream.getOrElse(true.B),
        indValue,
        curr + linDelta
      )
    case None =>
  }

  // Calculate updated Length 1D in 3D if 3D stream continues
  updL1Din3D match {
    case Some(next) =>
      require(currStrEntry.currLength1Din3D.isDefined)
      val curr: UInt = currStrEntry.currLength1Din3D.get
      next := curr + actualValueOfSignUInt(curr.getWidth, origStrEntry.stretch3D1D)
    case None =>
  }

  // Calculate updated Length 2D in 2D if 2D stream continues
  updL2Din2D match {
    case Some(next) =>
      require(currStrEntry.currLength2Din2D.isDefined && numFinish1DStrWire.isDefined)
      val curr: UInt = currStrEntry.currLength2Din2D.get
      next := curr - numFinish1DStrWire.get
    case None =>
  }

  // Calculate updated Length 2D in 3D if 3D stream continues
  updL2Din3D match {
    case Some(next) =>
      require(currStrEntry.currLength2Din3D.isDefined)
      val curr: UInt = currStrEntry.currLength2Din3D.get
      next := curr + actualValueOfSignUInt(curr.getWidth, origStrEntry.stretch3D2D)
    case None =>
  }

  // Calculate updated Length 3D if 3D stream continues
  updL3D match {
    case Some(next) =>
      require(currStrEntry.currLength3D.isDefined && numFinish2DStrWire.isDefined)
      val curr: UInt = currStrEntry.currLength3D.get
      next := curr - numFinish2DStrWire.get
    case None =>
  }

  // Calculate updated Stride 2D in 3D if 3D stream continues
  updStride2D match {
    case Some(next) =>
      require(currStrEntry.currStride2D.isDefined)
      val curr: UInt = currStrEntry.currStride2D.get
      next := curr + actualValueOfSignUInt(curr.getWidth, origStrEntry.deltaStride2D)
    case None =>
  }

  // Calculate updated Stretch 2D in 3D if 3D stream continues
  updStretch2D match {
    case Some(next) =>
      require(currStrEntry.currStretch2D.isDefined)
      val curr: UInt = currStrEntry.currStretch2D.get
      next := curr + actualValueOfSignUInt(curr.getWidth, origStrEntry.deltaStretch2D)
    case None =>
  }

  /* -------------------------     Finite State Machine     ------------------------- */

  /* -------------------------       Output Connection      ------------------------- */

  /* ---------- Output to Updated Outstanding Stream Entry ---------- */

  // Original Stream Entry will not changed except the valid determined by stream alive
  val originalStrBits: Int = selStrEntry.currStrEntry.origStrEntry.getWidth
  require(updStrEntry.origStrEntry.getWidth == originalStrBits)
  updStrEntry.origStrEntry :=
    Cat(
      updStrAlive /* Valid is connected here*/,
      selStrEntry.currStrEntry.origStrEntry.asUInt().apply(originalStrBits - 2 /*Original Valid is removed*/, 0)
    )
      .asTypeOf(new StreamEntry(memNode, ivpsParam, ovpsParam)) // replace valid with stream alive

  // Updated Start Point in Linear 1D Stream
  updStrEntry.currStartPoint1D match {
    case Some(cSP1D) =>
      require(updStartPoint1D.isDefined)
      cSP1D :=
        Mux(
          !linear1DLast.getOrElse(false.B),
          updStartPoint1D.get, // L1D continues
          // L1D finished, but L2D continues
          Mux(
            !linear2DLast.getOrElse(false.B),
            updStartPoint2D.getOrElse(origStrEntry.startPoint.getOrElse(0.U)),
            // L2D finished, but L3D continues
            Mux(
              !linear3DLast.getOrElse(false.B),
              updStartPoint3D.getOrElse(origStrEntry.startPoint.getOrElse(0.U)),
              0.U // Stream finished
            )
          )
        )
    case None => require(selStrEntry.currStrEntry.currStartPoint1D.isEmpty)
  }

  // Updated Start Point in Linear 2D Stream
  updStrEntry.currStartPoint2D match {
    case Some(cSP2D) =>
      require(updStartPoint2D.isDefined)
      cSP2D := {
        val next: UInt = Mux(
          !linear2DLast.getOrElse(false.B),
          updStartPoint2D.get, // L2D continues
          Mux(
            !linear3DLast.getOrElse(false.B),
            updStartPoint3D.getOrElse(origStrEntry.startPoint.getOrElse(0.U)), // L2D finished, but L3D continues
            0.U
          ) // Stream Finished
        )
        Mux(linear1DLast.getOrElse(false.B), next, currStrEntry.currStartPoint2D.get)
      }
    case None => require(selStrEntry.currStrEntry.currStartPoint2D.isEmpty)
  }

  // Updated Start Point in Linear 3D Stream
  updStrEntry.currStartPoint3D match {
    case Some(cSP3D) =>
      require(updStartPoint3D.isDefined)
      cSP3D := {
        val next: UInt = Mux(!linear3DLast.getOrElse(false.B), updStartPoint3D.get, 0.U)
        Mux(linear2DLast.getOrElse(false.B), next, currStrEntry.currStartPoint3D.get)
      }
    case None => require(selStrEntry.currStrEntry.currStartPoint3D.isEmpty)
  }

  // Updated Length1D in Linear 1D Stream
  updStrEntry.currLength1Din1D match {
    case Some(value) =>
      require(updL1Din1D.isDefined)
      value :=
        Mux(
          !linear1DLast.getOrElse(false.B),
          updL1Din1D.get,
          Mux(
            !linear2DLast.getOrElse(false.B),
            updL1Din2D.getOrElse(origStrEntry.initLength1D.getOrElse(0.U)),
            Mux(
              !linear3DLast.getOrElse(false.B),
              updL1Din3D.getOrElse(origStrEntry.initLength1D.getOrElse(0.U)),
              0.U // Stream Finished
            )
          )
        )
    case None => require(selStrEntry.currStrEntry.currLength1Din1D.isEmpty)
  }

  // Updated Length1D in Linear 2D Stream
  updStrEntry.currLength1Din2D match {
    case Some(value) =>
      require(updL1Din2D.isDefined)
      value := {
        val next: UInt = Mux(
          !linear2DLast.getOrElse(false.B),
          updL1Din2D.get,
          Mux(
            !linear3DLast.getOrElse(false.B),
            updL1Din3D.getOrElse(origStrEntry.initLength1D.getOrElse(0.U)),
            0.U // Stream Finished
          )
        )
        Mux(linear1DLast.getOrElse(false.B), next, currStrEntry.currLength1Din2D.get)
      }
    case None => require(selStrEntry.currStrEntry.currLength1Din2D.isEmpty)
  }

  // Updated Length1D in Linear 3D Stream
  updStrEntry.currLength1Din3D match {
    case Some(value) =>
      require(updL1Din3D.isDefined)
      value := {
        val next: UInt = Mux(!linear3DLast.getOrElse(false.B), updL1Din3D.get, 0.U)
        Mux(linear2DLast.getOrElse(false.B), next, currStrEntry.currLength1Din3D.get)
      }
    case None => require(selStrEntry.currStrEntry.currLength1Din3D.isEmpty)
  }

  // Updated Length2D in Linear 2D Stream
  updStrEntry.currLength2Din2D match {
    case Some(value) =>
      require(updL2Din2D.isDefined)
      value := {
        val next: UInt = Mux(
          !linear2DLast.getOrElse(false.B),
          updL2Din2D.get,
          Mux(
            !linear3DLast.getOrElse(false.B),
            updL2Din3D.getOrElse(origStrEntry.initLength2D.getOrElse(0.U)),
            0.U // Stream Finished
          )
        )
        Mux(linear1DLast.getOrElse(false.B), next, currStrEntry.currLength2Din2D.get)
      }
    case None => require(selStrEntry.currStrEntry.currLength2Din2D.isEmpty)
  }

  // Updated Length2D in Linear 3D Stream
  updStrEntry.currLength2Din3D match {
    case Some(value) =>
      require(updL2Din3D.isDefined)
      value := {
        val next: UInt = Mux(!linear3DLast.getOrElse(false.B), updL2Din3D.get, 0.U)
        Mux(linear2DLast.getOrElse(false.B), next, currStrEntry.currLength2Din3D.get)
      }
    case None => require(selStrEntry.currStrEntry.currLength2Din3D.isEmpty)
  }

  // Updated Length3D in Linear 3D Stream
  updStrEntry.currLength3D match {
    case Some(value) =>
      require(updL3D.isDefined)
      value := {
        val next: UInt = Mux(!linear3DLast.getOrElse(false.B), updL3D.get, 0.U)
        Mux(linear2DLast.getOrElse(false.B), next, currStrEntry.currLength3D.get)
      }
    case None => require(selStrEntry.currStrEntry.currLength3D.isEmpty)
  }

  // Updated Stride2D in Linear 3D Stream
  updStrEntry.currStride2D match {
    case Some(value) =>
      require(updStride2D.isDefined)
      value := {
        val next: UInt = Mux(!linear3DLast.getOrElse(false.B), updStride2D.get, 0.U)
        Mux(linear2DLast.getOrElse(false.B), next, currStrEntry.currStride2D.get)
      }
    case None => require(selStrEntry.currStrEntry.currStride2D.isEmpty)
  }

  // Updated Stretch2D in Linear 3D Stream
  updStrEntry.currStretch2D match {
    case Some(value) =>
      require(updStretch2D.isDefined)
      value := {
        val next: UInt = Mux(!linear3DLast.getOrElse(false.B), updStretch2D.get, 0.U)
        Mux(linear2DLast.getOrElse(false.B), next, currStrEntry.currStretch2D.get)
      }
    case None => require(selStrEntry.currStrEntry.currStretch2D.isEmpty)
  }

  // Update Start of Stream, turn if off when it is on, will not be turn on again in AGU
  updStrEntry.StartOfStream :=
    Mux(origStrEntry.valid && currStrEntry.StartOfStream, false.B, currStrEntry.StartOfStream)

  // Update Start of Stream 1D: turn it on if Linear1D ends but stream not end
  updStrEntry.StartOf1D :=
    Mux(origStrEntry.valid && linear1DLast.getOrElse(false.B) && !linearEnd && updStrAlive, true.B, false.B)

  // Update Start of Stream 2D: turn it on if Linear2D ends but stream not end
  updStrEntry.StartOf2D :=
    Mux(origStrEntry.valid && linear2DLast.getOrElse(false.B) && !linearEnd && updStrAlive, true.B, false.B)

  // Output to the request port
  strReqPorts.zipWithIndex.foreach { case (reqPort, reqIdx) =>
    // Get valid of indirect port
    val indirectValid: Bool = indirectAddrValids match {
      case Some(valids) => Mux(origStrEntry.indirectIdxStream.getOrElse(false.B), valids(reqIdx), true.B)
      case None         => true.B
    }
    // Request Valid
    reqPort.valid := origStrEntry.valid && !zeroLength && indirectValid
    // Connect Meta Info
    entry2meta(origStrEntry, reqPort.meta)
    // Connect Stream State
    reqPort.state match {
      case Some(state) =>
        val e1d: Bool = linear1DLast.getOrElse(false.B)
        val s1d: Bool = currStrEntry.StartOf1D
        state.End1D := e1d
        state.Start1D := s1d
        // Higher dimension is gated by lower one
        state.End2D := e1d && linear2DLast.getOrElse(false.B)
        state.Start2D := s1d && currStrEntry.StartOf2D
        state.EndStr := e1d && !updStrAlive
        state.StartStr := s1d && currStrEntry.StartOfStream
      case None =>
    }
    // Connect Address
    optConnect(reqPort.addr, if (reqAddresses.isDefined) Some(reqAddresses.get.apply(reqIdx)) else None)
    // Connect Mask
    optConnect(reqPort.mask, if (reqBitmasks.isDefined) Some(reqBitmasks.get.apply(reqIdx)) else None)
    // Connect Write Data
    optConnect(reqPort.writeData, if (reqWriteDataWire.isDefined) Some(reqWriteDataWire.get.apply(reqIdx)) else None)
  }

  /* -------------------------     Hardware Sanity Check    ------------------------- */

  /* -------------------------            Utility           ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */
  if (indirectBitmask.isDefined) {
    require(indirectBitmask.get.getWidth == memNode.bandwidth, s"width of bitmask is not memory bandwidth")
  }
}
