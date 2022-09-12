package dsagen2.util

import chisel3._
import chisel3.util._
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle._
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.util.UIntUtil.groupBitsAs

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

object StreamUtil {

  /**
   * This implicit function is used to convert optional input into 1.0
   * @param opt optional input
   * @tparam T Type of option input
   * @return If opt is Some(_), return 1; otherwise return 0
   */
  implicit def some2int[T](opt : Option[T]) : Int =
    opt match{
      case Some(_) => 1
      case None => 0
    }

  def getBitsFromMaxAbs(maxAbs : Int) : Int = {
    if(maxAbs == 0){
      0
    }else if(maxAbs == 1){
      1
    }else if(maxAbs > 1){
      log2Ceil(2 * maxAbs - 1)
    }else{
      require(requirement = false, s"Max Abs cannot be negative = $maxAbs")
      -1
    }
  }

  /**
   * Assign to wire of stream entry from stream dispatch bus
   * @param strEntry Stream Entry Wire
   * @param dispBus Stream Dispatch Bus
   */
  def disp2stream(strEntry : StreamEntry, dispBus : StreamDispatchBus) : Unit = {
    val strFields : ListMap[String, Data] = strEntry.elements
    val dispFields : ListMap[String, Data] = dispBus.elements
    strFields.foreach{
      case (fieldName, fieldHardware) =>
        require(dispFields.isDefinedAt(fieldName),
          s"How can dispatch bus not contain $fieldName, " +
            s"Sihao must do something wrong, here are the full name set of" +
            s"dispatch bus: ${dispFields.keys}")
        fieldHardware := dispFields(fieldName)
    }
  }

  /**
   * Calculate the actual UInt value of an Signed UInt (I know it makes no sense)
   *
   * So here is the problem: things like Stride2D, if it is 1-bit, it means 0.U and 1.U if it is UInt. But if we
   * make it SInt, a 1-bit SInt means 0.S and -1.S. So we want 1-bit delta to be UInt, but delta with more than
   * 1-bit will be treated as SInt and do sign extending to targetBits wide
   *
   * @param targetBits The target bit wide
   * @param delta an optional UInt which is actually SInt when its bitwidth is more than 1
   * @return The Signed Delta in UInt form but it can represent (0.U and 1.U) when targetBits = 1
   */
  def actualValueOfSignUInt(targetBits : Int, delta : Option[UInt]) : UInt = {
    // Create the target wire
    val sameWidthDelta : UInt = WireInit(0.U(targetBits.W))
    delta match {
      case Some(value) =>
        if(value.getWidth > 1) {
          // Create same width SInt wire
          val sameWidthSignDelta : SInt = WireInit(0.S(targetBits.W))
          sameWidthSignDelta := value.asSInt() // Sign extending here
          sameWidthDelta := sameWidthSignDelta.asUInt()
        } else{
          sameWidthDelta := value
        }
      case None =>
    }
    // Return
    sameWidthDelta
  }

  // Calculate Absolute Value for Signed UInt (1.W is UInt, n.W is SInt if n > 1)
  def absSignUInt(su : UInt) : UInt = {
    if(su.getWidth == 1){
      su
    }else{ // width > 1
      val width : Int = su.getWidth
      val result : UInt = WireInit(0.U(width.W))

      val isNeg : Bool = su(width - 1).asBool()
      val neg : UInt = -su
      result := Mux(isNeg, neg, su)
      result
    }
  }


  // Similar function like above but take data type into account
  def actualValueOfSignUIntWithDataType(targetBits : Int,           delta : Option[UInt],
                                        dataTypeExp : Option[UInt], numDataType : Int) : UInt = {
    // Sanity Check
    require(delta.isDefined)
    require(delta.get.getWidth >= 8, s"Delta required data type should be at least 8 bit")
    require(numDataType > 0)
    // Create target wire
    val sameWidthDelta : UInt = WireInit(0.U(targetBits.W))
    // Create result of different data type
    val resLUT : Seq[(UInt, UInt)] = for(exp <- 0 until numDataType) yield {
      val numBits : Int = 8 * (1 << exp)
      require(numBits <= delta.get.getWidth, s"Data Type specified is larger than actual delta")
      // Get the lower bit of delta as Signed
      val lowerBits : SInt = delta.get.apply(numBits - 1, 0).asSInt()
      // Sign extending
      val sameWidthSignDelta : SInt = WireInit(0.S(targetBits.W))
      sameWidthSignDelta := lowerBits
      // Return as UInt mapping
      exp.U -> sameWidthSignDelta.asUInt()
    }
    // Select based on dataType
    sameWidthDelta := MuxLookup(dataTypeExp.getOrElse(0.U),0.U,resLUT)
    // Return
    sameWidthDelta
  }

  // Get single element mask
  // Generate the mask for single element
  def singelElemRowMask(addr : UInt, dataTypeExp : Option[UInt], numDataType : Int, width : Int) : UInt = {
    // Sanity Check
    require(width > 1)
    // Create result wire
    val result : UInt = WireInit(0.U(width.W))
    if(width > 1){
      // local offset
      val localOff : UInt = addr(log2Ceil(width) -1, 0)
      val localOffBits : Int = localOff.getWidth
      // generate bitmask for each data type
      val maskLUT : Seq[(UInt, UInt)] = for(exp <- 0 until numDataType) yield{
        val mask : Seq[Bool] = for(idx <- 0 until width) yield {
          if(exp < localOffBits){
            val idxUInt = idx.U(localOffBits.W)
            localOff(localOffBits - 1, exp) === idxUInt(localOffBits - 1, exp)
          }else{
            true.B
          }
        }
        exp.U -> VecInit(mask).asUInt()
      }
      // Assign
      result := MuxLookup(dataTypeExp.getOrElse(0.U), 0.U, maskLUT)
    }else{ // width == 1
      result := 1.U(1.W)
    }
    // Return
    result
  }

  // Vector of connect
  def vecConnect[T <: Data](sinkVec : Vec[T], sourceVec : Vec[T], widthCheck : Boolean = true) : Unit = {
    require(sinkVec.length == sourceVec.length)
    sinkVec.zip(sourceVec).foreach{ case (sink, source) =>
      if(widthCheck) {
        require(sink.getWidth == source.getWidth, s"Assign between different width: " +
          s"sink = ${sink.getWidth}-bit, source = ${source.getWidth}-bit")
      }
      sink := source
    }
  }

  // Option Connect
  def optConnect[T <: Data](sink : Option[T], source : Option[T],
                            widthCheck : Boolean = true, existCheck : Boolean = true) : Unit = {
    // Existence check
    if(existCheck){
      (sink, source) match {
        case (Some(_), Some(_)) =>
        case (None, None) =>
        case _ => require(requirement = false, s"Existence is problematic: " +
          s"Defined? Source: ${source.isDefined}; Sink: ${sink.isDefined}")
      }
    }
    // Assign
    (sink, source) match {
      case (Some(s), Some(k)) =>
        if(widthCheck) require(s.getWidth == k.getWidth, s"Width check fail: " +
          s"source = ${s.getWidth}-bit, sink = ${k.getWidth}-bit")
        s := k
      case _ =>
    }
  }

  // Connect Stream Meta from Stream Entry, Meta is just short version of stream entry
  def entry2meta(entry : StreamEntry, meta : StreamMeta) : Unit = {
    // make sure that stream entry and stream meta is from same memory node
    require(entry.memNode == meta.memNode)
    // MStatus
    (meta.mStatus, entry.mStatus) match {
      case (Some(meta), Some(entry)) => meta := entry
      case (Some(_), None) => require(requirement = false, s"Meta want MStatus, but your entry cannot provide")
      case (None, Some(_)) =>
      case (None, None) =>
    }
    // Target Port
    if(entry.memNode.isREC){
      optConnect(meta.targetLocalPortId, entry.recIVPortId) // Recurrence stream should change target port here
    }else{
      optConnect(meta.targetLocalPortId, entry.targetLocalPortId)
    }
    // Data Type for Address Generation
    optConnect(meta.memDataTypeExp, entry.memDataTypeExp)
    // Data Type for Sequence Generation
    optConnect(meta.constDataTypeExp, entry.constDataTypeExp)
    // Memory Operation
    optConnect(meta.memOperation, entry.memOperation)
    // Stream Pattern Type linear indirect
    optConnect(meta.LinOrInd, entry.LinOrInd)
    // Linear Padding
    optConnect(meta.linearPadding, entry.linearPadding)
    // Connect whether it is ascending or descending
    meta.isDescend match {
      case Some(descend) =>
        require(entry.stride1D.isDefined)
        require(entry.stride1D.get.getWidth > 1)
        // The highest bit is sign bit, 1 means negative number which means descending stream
        descend := entry.stride1D.get.apply(entry.stride1D.get.getWidth - 1).asBool()
      case None =>
        require(entry.stride1D.isEmpty || (entry.stride1D.isDefined && entry.stride1D.get.getWidth == 1))
    }
  }

  // Convert stream request directly to stream response by using write data as read data (recurrence)
  def strReq2Rsp(sink: StreamResponse, source : StreamRequest) : Unit = {
    // Connect Valid
    sink.valid := source.valid
    // Connect meta info
    sink.meta := source.meta
    // Connect stream state
    sink.state match {
      case Some(state) => require(source.state.isDefined)
        state := source.state.get
      case None =>
    }
    // Connect address, although I think it is useless
    sink.addr match {
      case Some(addr) => addr := source.addr.getOrElse(0.U)
      case None =>
    }
    // Connect mask, I think this should aggregated to the lower position
    sink.mask match {
      case Some(mask) => mask := source.mask.getOrElse(0.U)
      case None => require(requirement = false, "First this function should be called for recurrence engine," +
        "and for recurrence engine, I think mask is required")
    }
    // Connect Data, this is where the recurrence happen
    sink.readData match {
      case Some(data) => data := source.writeData.getOrElse(0.U)
      case None => require(requirement = false, "First this function should be called for recurrence engine," +
        "and for recurrence engine, I think mask is required")
    }
  }

  // Connect Stream Request to Memory Request
  def strReq2memReq(strReq:StreamRequest, memReq : MemRequest) : Unit = {
    memReq.valid := strReq.valid
    memReq.vaddr := strReq.addr.getOrElse(0.U)
    memReq.memOp := strReq.meta.memOperation.getOrElse(memOpAtomOp5)
    (memReq.mStatus, strReq.meta.mStatus) match{
      case (Some(mStatus), Some(sStatus)) => mStatus := sStatus
      case (Some(mStatus), None) =>
        require(requirement = false, s"Memory request asking for mStatus but stream cannot provide on")
      case (None, Some(sStatus)) =>
      case (None, None) =>
    }
    memReq.memDataTypeExp match {
      case Some(value) => value := strReq.meta.memDataTypeExp.getOrElse(0.U)
      case None =>
    }
    require(memReq.mask.getWidth == strReq.mask.getOrElse(0.U(memReq.mask.getWidth.W)).getWidth)
    memReq.mask := strReq.mask.getOrElse(0.U)
    require(memReq.data.getWidth == strReq.writeData.getOrElse(0.U(memReq.data.getWidth.W)).getWidth)
    memReq.data := strReq.writeData.getOrElse(0.U)
  }

  // This function clear the lower bits of physical address to align to each full beatBytes
  def clearLowBits(paddr : UInt, loBits : Int) : UInt = {
    val higherBits : UInt = paddr(paddr.getWidth - 1, loBits)
    Cat(higherBits, 0.U(loBits.W))
  }

  // This function connects to two vector of tag value
  def vecDataConnect(sink : Vec[UInt], source : Vec[UInt]) : Unit = {
    // Get the vector width and data granularity for both side
    val sourceBits : Int = source.head.getWidth
    val sinkBits : Int = sink.head.getWidth
    // Sink and Source Width should be in multiple relation
    require(sinkBits % sourceBits == 0 || sourceBits % sinkBits == 0,
      s"Sink value bits $sinkBits and Source value bits $sourceBits is not in multiple relation")
    vecDataConnect(sink, source.asUInt())
  }

  // This function connect a UInt to vector of tag value, with predication always true
  def vecDataConnect(sink : Vec[UInt], source : UInt) : Unit = {
    // Get the vector width and data granularity for both side
    val sinkVecWidth : Int = sink.length
    val sinkUnitBits : Int = sink.head.getWidth
    // Extend source signal to sink width
    val extSource : UInt = WireInit(0.U(sink.getWidth.W)); extSource := source
    // Group the source bit as each element is sink value width
    val sourceGroupBits : Seq[UInt] = groupBitsAs(extSource, sinkUnitBits)
    // Connect sink and source
    if(sinkVecWidth <= sourceGroupBits.length){
      sink.zip(sourceGroupBits).foreach{case (sink, source) => require(sink.getWidth == source.getWidth); sink := source}
    }else{
      // Padded 0 at the end of vector
      val paddedVec : Seq[UInt] = sourceGroupBits ++ Seq.fill(sinkVecWidth - sourceGroupBits.length)(0.U(sinkUnitBits.W))
      require(sink.length == paddedVec.length)
      sink.zip(paddedVec).foreach{ case (sink, source) => require(sink.getWidth == source.getWidth);sink := source}
    }
  }

  // This function connect a vector of UInt to a UInt, just concat all value
  def mergeVecData(source : Seq[UInt]) : UInt = VecInit(source).asUInt()

  // Get the merged value of vector of tag value with suggest name for debug purpose
  def nameVecData(vtv : Seq[UInt], name : String) : UInt = {
    VecInit(vtv).asUInt().suggestName(name)
  }

  // Convert line address to bank address
  def lineAddr2bankAddr(lineAddr:UInt, bankIdx: Int, numBank:Int, bankWidth : Int) : UInt = {
    require(numBank > 0 && isPow2(numBank), s"Number of bank should be positive and power of 2, but it is $numBank")
    if(numBank == 1){
      lineAddr
    }else{
      // Calculate the number of bits required for bank selection and bank offset
      val bankSelBits : Int = log2Ceil(numBank)
      val bankOffBits : Int = log2Ceil(bankWidth)
      // Sanity check
      require(lineAddr.getWidth >= bankSelBits + bankOffBits,
        s"line address is ${lineAddr.getWidth} bits, but $numBank banks with $bankWidth width need at least " +
          s"${bankSelBits + bankOffBits} bits")
      // Extract the line selection and bank offset
      val lineSel : UInt = lineAddr(lineAddr.getWidth - 1, bankSelBits + bankOffBits)
      val bankOff : UInt = lineAddr(bankOffBits - 1, 0)
      val bankSel : UInt = bankIdx.U(bankSelBits.W)
      Cat(lineSel, bankSel, bankOff)
    }
  }

  // Reduce to calculate minimum with valid bit
  def validReduceMin(seq: Seq[(Bool, UInt)]) : (Bool, UInt) = {
    // valid
    val valid : Bool = VecInit(seq.map(_._1)).asUInt().orR()
    // uint
    val (_, value) = seq.drop(1).foldLeft(seq.head){
      case ((b1, v1), (b2, v2)) => (b1 || b2, Mux(b1 && b2, v1.min(v2), Mux(b1, v1, Mux(b2, v2, 0.U))))
    }
    (valid, value)
  }
}
