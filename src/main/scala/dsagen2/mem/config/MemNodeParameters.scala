package dsagen2.mem.config

import chisel3.UInt
import chisel3.util.{isPow2, log2Ceil}
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.top.diplomacy.WithNodeIDParameters
import dsagen2.util.StreamUtil.getBitsFromMaxAbs
import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json.{JsObject, JsValue, Json, Writes}

/** CDE Parameter for Memory Node contains two parts: Memory Node + Stream Pattern.
  *
  * Memory Node Part is common properties and Stream Pattern Part is memory node specific
  * TODO: we should separate the stream pattern part to specific memory node later
  *
  * @param nodeId                 Unique ID for Node Parameter, -1 is unset
  * @param nodeType               DMA or SPM or REC or DIS or GEN or REG
  * @param memUnitBits            Minimum cell of memory, in unit of bit; For byte-addressable memory, it is 8
  *                               Actually this is min(minimum_granularity_of_stream, minimum_granularity_of_memory)
  *                               DMA: Min(MainMemoryCellBitwidth, SupportedMinimumStreamDataType)
  *                               SPM: Min(SpadMemoryCellBitwidth, SupportedMinimumStreamDataType)
  *                               REC: SupportedMinimumStreamDataType
  *                               DIS: SupportedMinimumStreamDataType
  *                               GEN: SupportedMinimumStreamDataType
  *                               REG: XLEN
  * @param numMemDataTypeExp      Number of Memory Stream Data Type that Memory Node can support
  *                               This is purely defined by users. For example, if memUnitBits = 8, numMemUnitBitsExp 4.
  *                               It means that we support byte/half-word/word/double-word in total four kinds of stream.
  * @param numRead                Number of read port, concurrency for read request
  *                               DMA: 1 (Stream Sent to 1 InputVectorPort at a time, arbitrate at SystemBus)
  *                               SPM: NumSpmReadPort >= 1
  *                               REC: NumRecReadPort >= 1
  *                               DIS: N/A
  *                               GEN: NumGenReadPort >= 1
  *                               REG: 1
  * @param numWrite               Number of write port, concurrency for write request
  *                               DMA: 1 (Stream Sent to 1 InputVectorPort at a time, arbitrate at SystemBus)
  *                               SPM: NumSpmWritePort >= 1
  *                               REC: NumRecWritePort >= 1
  *                               DIS: NumDisWritePort >= 1
  *                               GEN: N/A
  *                               REG: 1
  * @param readWidth              Read Bandwidth in unit of `memory_unit_bits` *per read port* *per cycle*
  *                               DMA: SystemBusWidth
  *                               SPM: SpadMemoryBandWidth
  *                               REC: RecurrenceBusWidth
  *                               DIS: N/A (Do not support read)
  *                               GEN: MaxInputVectorPortBitWidth
  *                               REG: 4/8 (means 32 or 64 bits)
  * @param writeWidth             Write Bandwidth in unit of `memory_unit_bits` *per write port* *per cycle*
  *                               DMA: SystemBusWidth
  *                               SPM: SpadMemoryBandWidth
  *                               REC: RecurrenceBusWidth
  *                               DIS: MaxOutputVectorPortBitWidth
  *                               GEN: N/A (Do not support write)
  *                               REG: 4 / 8 (32/64 bits)
  * @param capacity               Capacity of Memory in unit of `memory_unit_bits`, `memory_unit_bits`-addressable memory
  *                               DMA: Accessible Main Memory Size
  *                               SPM: Scratchpad Memory Size
  *                               REC: 0 (should not used)
  *                               DIS: Infinity (should not used)
  *                               GEN: Infinity (should not used)
  *                               REG: 1 (should not used)
  * @param streamStated           Whether memory node will attach the state of stream for each memory READ request
  *                               The set of stream state is defined by the stream pattern parameter
  *                               DMA: Optional (up to full set)
  *                               SPM: Optional (up to full set)
  *                               REC: Optional (only StreamStart and StreamEnd), TODO: we should discuss more about it
  *                               DIS: None
  *                               GEN: Optional (up to full set)
  *                               REG: None
  * @param LinearPadding          Support Linear Padding Mode, only DMA and SPM
  * @param MaxLength1D            Max Length of Linear 1D stream
  * @param MaxAbsStride1D         Max Absolute Stride 1D
  * @param MaxLength2D            Max Length of Linear 2D stream
  * @param MaxAbsStretch2D        Max Absolute Stretch 2D
  * @param MaxAbsStride2D         Max Absolute Stride 2D
  * @param MaxLength3D            Max length of Linear 3D stream
  * @param MaxAbsDeltaStride2D    Max Absolute Delta of Stride 2D
  * @param MaxAbsDeltaStretch2D   Max Absolute Delta of Stretch 2D
  * @param MaxAbsStretch3D2D      Max Absolute Stretch 3D to 2D
  * @param MaxAbsStretch3D1D      Max Absolute Stretch 3D to 1D
  * @param MaxAbsStride3D         Max Absolute Stride 3D
  * @param IndirectIndexStream    Enable index stream from output vector port
  * @param LinearStride2DStream   Enable Linear Stride 2D stream derived by Stride 2D (i x Stride2D)
  * @param IndirectStride2DStream Enable Indirect Stride 2D stream from output vector port
  * @param LinearLength1DStream   Enable Linear Length 1D stream derived by Stretch 2D (Length1D + i x Stretch2D)
  * @param IndirectLength1DStream Enable Indirect Length 1D stream from output vector port
  * @param AtomicOperations       Atomic operation support by this memory node
  */
case class MemNodeParameters( // Node Identification
  nodeId:   Int = -1,
  nodeType: DSAGenNodeType = DirectMemoryAccess,
  // Memory Granularity
  memUnitBits:       Int = 8, // Minimum Cell of this memory
  numMemDataTypeExp: Int = 4, // How many kind of stream supported by this memory

  // Basic Specification of this memory node
  numRead:    Int = 1, // number of read port
  numWrite:   Int = 1, // number of write port
  readWidth:  Int = 8, // how many memory unit can be read per cycle
  writeWidth: Int = 8, // how many memory unit can be written per cycle
  capacity:   Long = 16 * 1024, // number of memory unit can be stored

  // Meta info supported by this memory
  streamStated: Boolean = true, // produce the state of stream for vector issue

  // Linear Stream Pattern

  /* Linear Meta Information */
  LinearPadding: Boolean = true, // Enable Linear Padding
  /* Linear 1D Parameters */
  MaxLength1D: Int = Int.MaxValue - 1, // Upper bound of Length 1D (=), 0 means disabled
  MaxAbsStride1D: Int =
    1, //Int.MaxValue / 2 + 1, // distance of inner most loop, 2 = ascend/const/descend, 1=const/ascend
  /* Linear 2D Parameters */
  MaxLength2D:     Int = Int.MaxValue - 1, // Upper bound of Length 2D (=), 0 means linear 2D is disabled
  MaxAbsStretch2D: Int = Int.MaxValue / 2 - 1, // Delta to the Length 1D, 0 means disabled
  MaxAbsStride2D:  Int = Int.MaxValue / 2 - 1, // Delta to the start point of Linear 1D stream, 0 means disabled
  /* Linear 3D Parameters */
  MaxLength3D:          Int = Int.MaxValue - 1, // Upper bound of Length 3D (=), 0 means linear 3D is disabled
  MaxAbsDeltaStride2D:  Int = Int.MaxValue / 2 - 1, // Delta to Stride 2D, 0 means disabled
  MaxAbsDeltaStretch2D: Int = Int.MaxValue / 2 - 1, // Delta to Stretch 2D, 0 means disabled
  MaxAbsStretch3D2D:    Int = Int.MaxValue / 2 - 1, // Delta to Length 2D per 2D stream, 0 means disabled
  MaxAbsStretch3D1D:    Int = Int.MaxValue / 2 - 1, // Delta to Length 1D per 1D stream, 0 means disabled
  MaxAbsStride3D:       Int = Int.MaxValue / 2 - 1, // Delta to start point to Linear 1D stream, 0 means disabled

  // Indirect Stream Pattern

  /* Indirect 1D Parameters */
  IndirectIndexStream: Boolean = true, // Index stream from output vector port or nothing
  NumIdxDataTypeExp:   Int = 4, // Number of data type for index stream
  /* Indirect 2D Parameters */
  LinearStride2DStream:   Boolean = true, // Support linear mode of stride 2D stream
  IndirectStride2DStream: Boolean = true, // Support indirect stride 2D stream from port
  NumStride2DUnitBitsExp: Int = 4, // Number of data type for stride2D stream
  LinearLength1DStream:   Boolean = true, // Support linear mode of length 1D stream
  IndirectLength1DStream: Boolean = true, // Support indirect length 1D stream from port
  NumLength1DUnitBitsExp: Int = 4, // Number of data type for length1D stream

  // Atomic Update
  AtomicOperations: Set[DsaOperation] = Set(FixedAdd, FixedSub, FixedMin, FixedMax),
  // Support Buffet Or Not?
  supportBuffet: Boolean = false,
  // Constant Stream Data Type (Generate Engine only)
  numGenDataType: Int = 4,
  // Number of ROB Entry, DMA and Indirect SPM and GEN only
  numPendingRequest: Int = 16,
  // Number of Scratchpad Memory Bank
  numSpmBank: Int = 4,
  // Max Data Type supported (XLEN)
  XLEN: Int = 64)
    extends JSONParsableParameters
    with WithNodeIDParameters {

  /*---------- Derived Parameters ----------*/

  // Calculate the number of bit needed by address
  def addrBits: Int = {
    require(isDMA || isSPM || isGEN, s"Only DMA ans SPM is actual memory")
    if (nodeType == DirectMemoryAccess) {
      // TODO: please replace this in the future, for now it is hard-coded for RISC-V vaddr bits = 40
      require(log2Ceil(capacity) == 40, s"capacity = $capacity")
      40
    } else if (nodeType == ScratchpadMemory) {
      log2Ceil(capacity)
    } else if (nodeType == GenerateEngine || nodeType == RegisterEngine) {
      XLEN
    } else {
      require(requirement = false, s"Node $nodeType ask for memory address bits")
      -1
    }
  }

  // total read bandwidth bits per port per cycle
  def readBits: Int = readWidth * memUnitBits

  // total write bandwidth bits per port per cycle
  def writeBits: Int = writeWidth * memUnitBits

  // total read unit (usually in byte) per cycle
  def readUnits: Int = readWidth * numRead

  // total write unit (usually in byte) per cycle
  def writeUnits: Int = writeWidth * numWrite

  // Whether this memory node support descending stream
  def supportDescend: Boolean = MaxAbsStride1D > 1

  // Whether of not support indirect stream pattern
  def supportIndirect: Boolean = IndirectIndexStream || IndirectStride2DStream || IndirectLength1DStream

  // Count the number of indirect stream pattern dimension
  def numIndirectDimension: Int =
    if (IndirectStride2DStream || IndirectLength1DStream) 2
    else if (IndirectIndexStream) 1
    else {
      require(!supportIndirect, s"Indirect stream pattern is not supported, but definition is weird")
      0
    }

  // support linear 1D stream pattern
  def Linear1DEnabled: Boolean = MaxLength1D > 0

  // support linear 2D stream pattern
  def Linear2DEnabled: Boolean = MaxLength2D > 0 && Linear1DEnabled

  // support linear 3D stream pattern
  def Linear3DEnabled: Boolean = MaxLength3D > 0 && Linear2DEnabled

  // Whether or not this memory node needs an AGU
  // DMA/SPM : Calculate the real address
  // GEN : Calculate the numerical sequence
  // REC/DIS : Calculate remaining Length 1D
  def needAGU: Boolean = (isDMA || isSPM || isGEN || isREC || isDIS) && MaxLength1D > 0 && bandwidth > 1

  // Count the number of linear pattern dimension
  def numLinearDimension: Int =
    if (Linear3DEnabled) 3 else if (Linear2DEnabled) 2 else if (Linear1DEnabled) 1 else 0

  // Whether or not support linear stream pattern
  def supportLinear: Boolean = numLinearDimension > 0

  // Whether or not support atomic operation
  def supportAtomicOperation: Boolean = AtomicOperations.nonEmpty

  // Number of Atomic Operation
  def numAtomOp: Int = AtomicOperations.size

  // Get all supported data type by providing the number of exponential
  def supportDataTypes(numExp: Int, max: Int): Seq[Int] =
    (for (power <- 0 until numExp) yield {
      memUnitBits * (1 << power)
    }).filter(x => x <= max)

  // Get all supported data type
  def supportMemDataTypes(max: Int): Seq[Int] = supportDataTypes(numMemDataTypeExp, max)

  // Number of bit needed to encode data type of stream
  def memDataTypeBits: Int = log2Ceil(numMemDataTypeExp)

  // Number of bit needed to encode generated stream data type
  def constDataTypeBits: Int = {
    require(nodeType == GenerateEngine, s"This memory node type is $nodeType, why you want to call this function")
    log2Ceil(numGenDataType)
  }

  // The Bandwidth (in unit of memUnitWidth-bit) TODO: for now, unit is byte
  def bandwidth: Int = {
    require(
      readWidth == writeWidth || (readWidth == 0 || writeWidth == 0),
      s"You call bandwidth, but readWidth $readWidth != writeWidth $writeWidth, " +
        s"which width you want?"
    )
    require(isPow2(readWidth) || isPow2(writeWidth), s"Bandwidth should be power of 2")
    require(readWidth > 0 || writeWidth > 0, s"Non-positive bandwidth $readWidth?")
    val width: Int = readWidth.max(writeWidth)
    // Sanity check only to Register Engine
    if (nodeType == RegisterEngine) {
      require(width == 4 || width == 8, s"Register Node only support 32/64 bit, but bandwidth = $width")
    }
    // return
    width
  }

  // The number of bit in band
  def bandBits: Int = bandwidth * memUnitBits

  // Number of memory operation supported
  def numMemOperation: Int = {
    nodeType match {
      case DirectMemoryAccess => 2 + AtomicOperations.size // read, write and atomic operation
      case ScratchpadMemory   => 2 + AtomicOperations.size // like DMA
      case RecurrenceEngine   => 0 // just recurrence
      case DiscardEngine      => 0 // just discard
      case GenerateEngine     => 0 // just generate
      case RegisterEngine     => 2 // read (cpu2ivp) or write (ovp2cpu)
      case errType: Any =>
        require(requirement = false, s"Node $errType is not memory node type")
        0
    }
  }

  // Number of bit needed to encode memory operation
  def memOperationBits: Int = if (numMemOperation > 1) log2Ceil(numMemOperation) else 0

  // Number of bit needed to encode Stride 1D
  def stride1DBits: Int = if (MaxAbsStride1D <= 0) 0 else getBitsFromMaxAbs(MaxAbsStride1D)

  // Number of bit needed to encode Stride 2D
  def stride2DBits: Int = if (MaxAbsStride2D <= 0) 0 else getBitsFromMaxAbs(MaxAbsStride2D)

  // Number of bit needed to encode Stride 3D
  def stride3DBits: Int = if (MaxAbsStride3D <= 0) 0 else getBitsFromMaxAbs(MaxAbsStride3D)

  // Number of bit needed to encode Stretch 2D
  def stretch2DBits: Int = if (MaxAbsStretch2D <= 0) 0 else getBitsFromMaxAbs(MaxAbsStretch2D)

  // Number of bit needed to encode Stretch 3D to 2D
  def stretch3D2DBits: Int = if (MaxAbsStretch3D2D <= 0) 0 else getBitsFromMaxAbs(MaxAbsStretch3D2D)

  // Number of bit needed to encode Stretch 3D to 1D
  def stretch3D1DBits: Int = if (MaxAbsStretch3D1D <= 0) 0 else getBitsFromMaxAbs(MaxAbsStretch3D1D)

  // Number of bit needed to encode delta stride 2D
  def deltaStride2DBits: Int = if (MaxAbsDeltaStride2D <= 0) 0 else getBitsFromMaxAbs(MaxAbsDeltaStride2D)

  // Number of bit needed to encode delta stretch 2D
  def deltaStretch2DBits: Int = if (MaxAbsDeltaStretch2D <= 0) 0 else getBitsFromMaxAbs(MaxAbsDeltaStretch2D)

  // Number of bit needed to encode Length 1D
  def length1DBits: Int = if (MaxLength1D > 0) log2Ceil(MaxLength1D + 1) else 0

  // Number of bit needed to encode Length 2D
  def length2DBits: Int = if (MaxLength2D > 0) log2Ceil(MaxLength2D + 1) else 0

  // Number of bit needed to encode Length 3D
  def length3DBits: Int = if (MaxLength3D > 0) log2Ceil(MaxLength3D + 1) else 0

  // Support Dual Mode for Stride 2D stream
  def dualModeStride2D: Boolean = LinearStride2DStream && IndirectStride2DStream

  // Support Dual Mode for Length 1D stream
  def dualModeLength1D: Boolean = LinearLength1DStream && IndirectLength1DStream

  // Calculate the number of bit needed by specifying the data type of index stream
  def idxStrDataTypeBits: Int = if (NumIdxDataTypeExp > 1) log2Ceil(NumIdxDataTypeExp) else 0

  // Calculate the number of bit needed by specifying the data type of Stride 2D stream
  def s2dStrDataTypeBits: Int = if (NumStride2DUnitBitsExp > 1) log2Ceil(NumStride2DUnitBitsExp) else 0

  // Calculate the number of bit needed by specifying the data type of Length 1D stream
  def l1dStrDataTypeBits: Int = if (NumLength1DUnitBitsExp > 1) log2Ceil(NumLength1DUnitBitsExp) else 0

  // Calculate the maximum number of bits for the index value from output vector port
  // for just ONE index value from output vector port
  def maxIndIndexBits: Int = {
    val max = if (NumIdxDataTypeExp > 0) supportDataTypes(NumIdxDataTypeExp, XLEN).max else 0
    require(max <= bandBits, s"Index Data Type = $max, but memory bandwidth = $bandBits-bit")
    max
  }

  // Calculate the max number of bits for ONE indirect stride 2D value from ovp
  def maxIndStride2DBits: Int = {
    val max = if (NumStride2DUnitBitsExp > 0) supportDataTypes(NumStride2DUnitBitsExp, XLEN).max else 0
    require(max <= bandBits, s"Stride2D Data Type = $max, but memory bandwidth = $bandBits-bit")
    max
  }

  // Calculate the max number of bits for ONE indirect length 1D value from ovp
  def maxIndLength1DBits: Int = {
    val max = if (NumLength1DUnitBitsExp > 0) supportDataTypes(NumLength1DUnitBitsExp, XLEN).max else 0
    require(max <= bandBits, s"Length Data Type = $max, but memory bandwidth = $bandBits-bit")
    max
  }

  // Number of Stream Request Port
  def numMemReqPort: Int = if (nodeType == ScratchpadMemory) numSpmBank else 1

  // SPM Only: Calculate the width of each bank in byte
  def spmBankWidth: Int = {
    require(nodeType == ScratchpadMemory, s"If the nodeType is not SPM, why you need to call this func")
    bandwidth / numSpmBank
  }

  // SPM Only: Calculate the bit width of each bank
  def spmBankBitWidth: Int = {
    memUnitBits * spmBankWidth
  }

  // Calculate the number of row in memory
  def numRow: Long = {
    val n: Long = capacity / bandwidth
    require(n > 0, s"Number of line ($n) need to be positive")
    n
  }

  // Calculate the lower position of row address
  def lowerPosRowAddr: Int = log2Ceil(bandwidth)

  // Whether or not need address in stream request
  // DMA and SPM need it for memory block access, GEN uses it as start point of numerical sequence
  def needAddr: Boolean = isDMA || isSPM || isGEN

  // Whether or nor can produce stream request
  def hasStrRequest: Boolean = isDMA || isSPM || isGEN || isREC

  // Whether or not need mask is stream response
  def needStrResponse: Boolean = isDMA || isSPM || isGEN || isREG || isREC

  // Whether a memory node needs OVP
  def needOVP: Boolean = isDMA || isSPM || (isGEN && supportIndirect) || isREG || isDIS || isREC

  // Whether a memory node can do write
  def canDoWrite: Boolean = isDMA || isSPM || isREC || isREG || isDIS

  // Function to check memory type
  def isDMA: Boolean = nodeType == DirectMemoryAccess

  def isSPM: Boolean = nodeType == ScratchpadMemory

  def isIndSPM: Boolean = isSPM && supportIndirect

  def isREC: Boolean = nodeType == RecurrenceEngine

  def isDIS: Boolean = false // Discard Engine to be removed

  def isGEN: Boolean = nodeType == GenerateEngine

  def isREG: Boolean = nodeType == RegisterEngine

  def hwMemType: UInt =
    nodeType match {
      case DirectMemoryAccess => dmaMemType
      case ScratchpadMemory   => spmMemType
      case RecurrenceEngine   => recMemType
      case DiscardEngine      => disMemType
      case GenerateEngine     => genMemType
      case RegisterEngine     => regMemType
      case errType: Any => require(requirement = false, s"$errType is not memory node type"); dmaMemType
    }

  // Whether or not need ROB
  def needROB: Boolean = isDMA || (isSPM && IndirectIndexStream)

  // Maximum Data Type Bits for Generated Stream
  def maxConstDataBits: Int = memUnitBits << (numGenDataType - 1)

  // Turn off Linear 3D
  def turnOffLinear3D: MemNodeParameters =
    this.copy(
      MaxLength3D = 0,
      MaxAbsDeltaStride2D = 0,
      MaxAbsDeltaStretch2D = 0,
      MaxAbsStretch3D2D = 0,
      MaxAbsStretch3D1D = 0,
      MaxAbsStride3D = 0
    )

  // Turn off Linear 2D
  def turnOffLinear2D: MemNodeParameters =
    this.turnOffLinear3D.copy(MaxLength2D = 0, MaxAbsStretch2D = 0, MaxAbsStride2D = 0)

  // Turn off linear 1D
  def turnOffLinear1D: MemNodeParameters = this.turnOffLinear2D.copy(MaxLength1D = 0, MaxAbsStride1D = 0)

  // Turn off Linear Padding
  def turnOffLinearPadding: MemNodeParameters = this.copy(LinearPadding = false)

  // Turn off all linear related feature
  def turnOffLinear: MemNodeParameters = this.turnOffLinear1D.turnOffLinearPadding

  // Turn off indirect index
  def turnOffIndirectIndex: MemNodeParameters = this.copy(IndirectIndexStream = false, NumIdxDataTypeExp = 0)

  // Turn off indirect length 1D
  def turnOffIndirectL1D: MemNodeParameters =
    this.copy(IndirectLength1DStream = false, LinearLength1DStream = false, NumLength1DUnitBitsExp = 0)

  // Turn off indirect stride 2D
  def turnOffIndirectS2D: MemNodeParameters =
    this.copy(IndirectStride2DStream = false, LinearStride2DStream = false, NumStride2DUnitBitsExp = 0)

  // Turn off all indirect feature
  def turnOffIndirect: MemNodeParameters =
    if (isSPM) this.copy(numPendingRequest = 0).turnOffIndirectIndex.turnOffIndirectL1D.turnOffIndirectS2D
    else this.turnOffIndirectIndex.turnOffIndirectL1D.turnOffIndirectS2D

  def sanity: Boolean = {
    /* ---------- Sanity Check ---------- */

    if (isSPM) {
      require(capacity <= 32 * 1024 * 1024, s"Scratchpad is $capacity Byte large")
    }

    // TODO: for now we only support byte-addressable memory
    require(memUnitBits == 8, s"We do not support memory that is not byte addressable, but it is $memUnitBits")

    // TODO: we do not support read and write has different bandwidth yet
    require(
      (readWidth > 0 || writeWidth > 0) || (readWidth == writeWidth || (readWidth == 0 || writeWidth == 0)),
      s"we do not support read and write has different bandwidth yet, " +
        s"readWidth = $readWidth, writeWidth = $writeWidth"
    )

    // At least, memory node has to support one data type
    require(numMemDataTypeExp >= 1, s"Memory node need to support at least one data type, but it is $numMemDataTypeExp")

    // Sanity check based on different kinds of memory node
    nodeType match {
      case DirectMemoryAccess =>
        // DMA Node supports the most complete set of feature, so there is no sanity check for now
        // Capacity should make sense
        require(
          capacity > 1 && isPow2(capacity),
          s"DMA size should be positive and power of 2, but" +
            s"it is $capacity $memUnitBits-bit"
        )
      case ScratchpadMemory =>
        // SPM Node supports the most complete set of feature, so there is no sanity check for now
        // Capacity should make sense
        require(
          capacity > 1 && isPow2(capacity),
          s"SPM size should be positive and power of 2, but" +
            s"it is $capacity $memUnitBits-bit"
        )
      case RecurrenceEngine =>
        // Recurrence Node can only support linear 1D pattern
        require(
          Linear1DEnabled && !Linear2DEnabled && !Linear3DEnabled && !supportIndirect,
          s"Recurrence engine only requires linear 1D stream pattern is enabled, but " +
            s"1D : $Linear1DEnabled; 2D : $Linear2DEnabled; 3D : $Linear3DEnabled"
        )
        // Sanity check: The number of read port and write port of recurrence engine should be same
        require(
          numRead == numWrite,
          s"The number of read / write port should be same, " +
            s"but #read = $numRead, #write = $numWrite"
        )
        // TODO: for now we only have one recurrence bus in recurrence engine
        require(numRead == 1, s"For now we only allow one recurrence bus, but it is $numRead")
      case DiscardEngine =>
        // Discard node can only support linear 1D pattern
        require(
          Linear1DEnabled && !Linear2DEnabled && !Linear3DEnabled && !supportIndirect,
          s"Discard engine only requires linear 1D stream pattern is enabled, but " +
            s"1D : $Linear1DEnabled; 2D : $Linear2DEnabled; 3D : $Linear3DEnabled"
        )
        // Discard engine is write-only
        require(numRead == 0, s"Discard Engine can only support write, but #read = $numRead")
      case GenerateEngine =>
        // Generate engine is read-only
        require(numWrite == 0, s"Generate Engine can only support read, but #write = $numWrite")
      case RegisterEngine =>
        // Register Engine has nothing to do with stream
        require(
          !supportIndirect && !supportLinear,
          s"Register engine has nothing to do with linear/indirect stream, " +
            s"but linear = $numLinearDimension, indirect = $numIndirectDimension"
        )
        // Check the read bits
        if (numRead != 0) {
          require(numRead == 1, s"Register Engine is 1 Read and 1 Write, but #read = $numRead")
          require(readBits == 32 || readBits == 64, s"I believe we can only send 32/64 bit data to ivp per cycle")
        }
        // Check the write bits
        if (numWrite != 0) {
          require(numWrite == 1, s"Register Engine is 1 Read and 1 Write, but #write = $numWrite")
          require(writeBits == 32 || writeBits == 64, s"I believe we can only take 32/64 bit data from ovp per cycle")
        }
        /*    TODO: for now we force the base memory unit bit to be 8, so we should not do this check
                ideally, memUnitBits for register engine should be equal to XLEN and number of supported data type is 1
              // For the register engine, the number of data type have to be one
              require(supportDataTypes.length == 1, s"Register Engine can only support one data type, which should be XLEN, " +
                s"but supported data types = $supportDataTypes")*/
        // For now, only 32/64 bit machine is supported
        require(
          supportMemDataTypes(XLEN).contains(32) || supportMemDataTypes(XLEN).contains(64),
          s"Register engine's supported data types contains something that is not 32/64 bits data type"
        )
      case errType: Any =>
        require(requirement = false, s"$errType is not memory node type")
    }

    // Sanity check: Specific check for generate engine
    nodeType match {
      case GenerateEngine =>
        // Generate engine should support at least one data type for constant stream (generated stream)
        require(numGenDataType >= 1, s"For generate engine, it must support as least one data type for const stream")
      case restNode: Any =>
        // For the rest of memory node, since they do not support generated stream, NumConstUnitBitsExp should be zero
        require(
          numGenDataType == 0,
          s"Memory Node $restNode does not support generated stream, " +
            s"but the number of constant stream data type is $numGenDataType"
        )
    }

    // Check for Number of Generated Engine
    require(
      (numGenDataType > 0 && nodeType == GenerateEngine) ||
        (numGenDataType == 0 && nodeType != GenerateEngine),
      s"Memory Node Type = $nodeType, but number of generated stream data type = $numGenDataType"
    )

    // If support atomic operation, it is either DMA or SPM, other memory node do not support this
    if (supportAtomicOperation) {
      require(nodeType == DirectMemoryAccess || nodeType == ScratchpadMemory)
    }

    // If the indirect pattern support linear generation of Stretch 2D, the related parameters should be enabled
    if (LinearLength1DStream) {
      require(
        Linear2DEnabled && MaxAbsStretch2D > 0,
        s"DSAGEN support linear pattern for Length 1D in indirect stream, " +
          s"but MaxLength2D = $MaxLength2D, MaxAbsStretch2D = $MaxAbsStretch2D"
      )
    }

    // If the indirect pattern support linear generation of Stride 2D, the related parameters should be enabled
    if (LinearStride2DStream) {
      require(
        Linear2DEnabled && MaxAbsStride2D > 0,
        s"DSAGEN support linear pattern for Stride 2D in indirect stream, " +
          s"but MaxLength2D = $MaxLength2D, MaxAbsStride2D = $MaxAbsStride2D"
      )
    }

    // If the linear 1D stream pattern is enabled, Stride 1D should make sense
    if (Linear1DEnabled) {
      require(MaxAbsStride1D >= 0, s"Absolute Stride 1D should not be negative if linear 1D is enabled")
    } else {
      require(MaxAbsStride1D == 0, s"Linear 1D stream is not enabled, but MaxAbsStride1D = $MaxAbsStride1D")
    }

    // Only DMA, SPM, GEN support Stride 1D, other nodes should be zero
    nodeType match {
      case DirectMemoryAccess =>
      case ScratchpadMemory   =>
      case RecurrenceEngine =>
        require(
          MaxAbsStride1D == 1,
          s"Recurrence Engine has to be ascending stream with " +
            s"stride 1D = 1, but Max Absolute Stride 1D = $MaxAbsStride1D"
        )
      case DiscardEngine  => require(MaxAbsStride1D == 1, s"Discard Engine does not support stride 1D")
      case GenerateEngine =>
      case RegisterEngine => require(MaxAbsStride1D == 0, s"Register Engine does not support stride 1D")
      case errType: Any => require(requirement = false, s"Node $errType is wrong")
    }

    // If the linear 2D stream pattern is enabled, all related MaxAbs parameters should not be negative
    if (Linear2DEnabled) {
      require(MaxAbsStretch2D >= 0, s"Linear 2D stream is enabled, but MaxAbsStretch2D = $MaxAbsStretch2D")
      require(MaxAbsStride2D >= 0, s"Linear 2D stream is enabled, but MaxAbsStride2D = $MaxAbsStride2D")
    } else {
      require(MaxAbsStretch2D == 0, s"Linear 2D is not enabled, but MaxAbsStretch2D = $MaxAbsStretch2D")
      require(MaxAbsStride2D == 0, s"Linear 2D is not enabled, but MaxAbsStride2D = $MaxAbsStride2D")
    }

    // If the linear 2D feature if enabled, then linear 2D must be enabled
    if (MaxAbsStretch2D > 0 || MaxAbsStride2D > 0) {
      require(
        Linear2DEnabled,
        s"MaxLength2D = $MaxLength2D, MaxAbsStretch2D = $MaxAbsStretch2D, MaxAbsStride2D = $MaxAbsStride2D, " +
          s"the combination does not make sense"
      )
    }

    // Only DMA, SPM and GEN can support linear pattern more than 2D
    if (
      MaxAbsStride2D > 0 || MaxAbsStretch2D > 0 || MaxLength2D > 1 ||
      MaxAbsStretch3D2D > 0 || MaxAbsStretch3D1D > 0 ||
      MaxAbsDeltaStride2D > 0 || MaxAbsDeltaStretch2D > 0 || MaxAbsStride3D > 0 || MaxLength3D > 1
    ) {
      require(
        Seq(DirectMemoryAccess, ScratchpadMemory, GenerateEngine).contains(nodeType),
        s"Only DMA, SPM and GEN can support linear pattern more than 2D, but this nodeType is $nodeType, " +
          s"MaxAbsStride2D = $MaxAbsStride2D, MaxAbsStretch2D = $MaxAbsStretch2D, MaxLength2D = $MaxLength2D, " +
          s"MaxAbsStretch3D2D = $MaxAbsStretch3D2D, MaxAbsStretch3D1D = $MaxAbsStretch3D1D," +
          s"MaxAbsDeltaStride2D = $MaxAbsDeltaStride2D, MaxAbsDeltaStretch2D = $MaxAbsDeltaStretch2D," +
          s"MaxAbsStride3D = $MaxAbsStride3D, MaxLength3D = $MaxLength3D"
      )
    }

    // If the linear 3D stream pattern is enabled, all related MaxAbs parameters should not be negative
    if (Linear3DEnabled) {
      require(MaxAbsDeltaStride2D >= 0, s"Linear 3D stream is enabled, but MaxAbsDeltaStride2D = $MaxAbsDeltaStride2D")
      require(
        MaxAbsDeltaStretch2D >= 0,
        s"Linear 3D stream is enabled, but MaxAbsDeltaStretch2D = $MaxAbsDeltaStretch2D"
      )
      require(MaxAbsStretch3D2D >= 0, s"Linear 3D stream is enabled, but MaxAbsStretch3D2D = $MaxAbsStretch3D2D")
      require(MaxAbsStretch3D1D >= 0, s"Linear 3D stream is enabled, but MaxAbsStretch3D1D = $MaxAbsStretch3D1D")
      require(MaxAbsStride3D >= 0, s"Linear 3D stream is enabled, but MaxAbsStride3D = $MaxAbsStride3D")
    } else {
      require(MaxAbsDeltaStride2D == 0, s"Linear 3D is not enabled, but MaxAbsDeltaStride2D = $MaxAbsDeltaStride2D")
      require(MaxAbsDeltaStretch2D == 0, s"Linear 3D is not enabled, but MaxAbsDeltaStretch2D = $MaxAbsDeltaStretch2D")
      require(MaxAbsStretch3D2D == 0, s"Linear 3D is not enabled, but MaxAbsStretch3D2D = $MaxAbsStretch3D2D")
      require(MaxAbsStretch3D1D == 0, s"Linear 3D is not enabled, but MaxAbsStretch3D1D = $MaxAbsStretch3D1D")
      require(MaxAbsStride3D == 0, s"Linear 3D is not enabled, but MaxAbsStride3D = $MaxAbsStride3D")
    }

    // If the linear 3D feature is enabled, then linear 3D must be enabled
    if (
      MaxAbsDeltaStride2D > 0 || MaxAbsDeltaStretch2D > 0 || MaxAbsStretch3D2D > 0 ||
      MaxAbsStretch3D1D > 0 || MaxAbsStride3D > 0
    ) {
      require(
        Linear3DEnabled,
        s"MaxAbsStretch3D2D = $MaxAbsStretch3D2D, MaxAbsStretch3D1D = $MaxAbsStretch3D1D," +
          s"MaxAbsDeltaStride2D = $MaxAbsDeltaStride2D, MaxAbsDeltaStretch2D = $MaxAbsDeltaStretch2D," +
          s"MaxAbsStride3D = $MaxAbsStride3D, MaxLength3D = $MaxLength3D combination does not make sense"
      )
    }

    // The linear 3D feature depends on corresponding linear 2D feature
    if (MaxAbsDeltaStride2D > 0) {
      require(
        MaxAbsStride2D > 0,
        s"MaxAbsDeltaStride2D = $MaxAbsDeltaStride2D, MaxAbsStride2D = $MaxAbsStride2D, " +
          s"Delta feature enabled, but base feature is not enabled"
      )
    }
    if (MaxAbsDeltaStretch2D > 0) {
      require(
        MaxAbsStretch2D > 0,
        s"MaxAbsDeltaStretch2D = $MaxAbsDeltaStretch2D, MaxAbsStretch2D = $MaxAbsStretch2D, " +
          s"Delta feature enabled, but base feature is not enabled"
      )
    }

    // If supporting linear padding, it means that memory node support linear stream pattern
    // In the future, we should support only a subset of linear padding modes
    if (LinearPadding) {
      // Only DMA, SPM and GEN can support this padding pattern
      require(
        nodeType != DiscardEngine,
        s"All nodes can support linear padding pattern except Discard, but this nodeType is $nodeType"
      )
      // TODO: for now, linear padding is by default consists of all 7 modes,
      //  which require at least linear 1D pattern
      require(numLinearDimension >= 1, s"Linear padding requires linear dimension is at least 1")
    }

    // Only DMA and SPM can support indirect stream
    if (supportIndirect) {
      require(
        isDMA || isSPM || isGEN,
        s"Only DMA or SPM or GEN can support indirect stream pattern, but node type is $nodeType"
      )
    }

    // If indirect mode and number of indirect stream data type should be consistent
    require(
      (NumIdxDataTypeExp > 0 && IndirectIndexStream) || (NumIdxDataTypeExp == 0 && !IndirectIndexStream),
      s"NumIdxUnitBitsExp = $NumIdxDataTypeExp, IndirectIndexStream = $IndirectIndexStream, this does not make sense"
    )
    require(
      (NumStride2DUnitBitsExp > 0 && IndirectStride2DStream) || (NumStride2DUnitBitsExp == 0 && !IndirectStride2DStream),
      s"NumStride2DUnitBitsExp = $NumStride2DUnitBitsExp, IndirectStride2DStream = $IndirectStride2DStream, this does not make sense"
    )
    require(
      (NumLength1DUnitBitsExp > 0 && IndirectLength1DStream) || (NumLength1DUnitBitsExp == 0 && !IndirectLength1DStream),
      s"NumLength1DUnitBitsExp = $NumLength1DUnitBitsExp, IndirectLength1DStream = $IndirectLength1DStream, this does not make sense"
    )

    // If indirect index stream is supported, it must be DMA or SPM
    if (IndirectIndexStream) {
      require(isDMA || isSPM || isGEN, s"Only DMA or SPM can support Indirect Index Stream, but it is $nodeType")
    }

    // Sanity Check: If the memory node is DMA, Number of Pending Request should be more than zero and be power of two
    if (isDMA || (isSPM && supportIndirect) || isGEN) {
      require(
        numPendingRequest > 1 && isPow2(numPendingRequest),
        s"Memory Node is DMA, The number of pending DMA request should be more than 1 and power of 2, but it is $numPendingRequest"
      )
    } else {
      require(
        numPendingRequest == 0,
        s"Memory Node Type is $nodeType, and support indirect = $supportIndirect" +
          s"why you need non-zero Number of pending request = $numPendingRequest?"
      )
    }

    // Sanity Check: #ScratchpadBank should consistent with nodeType
    require(
      (nodeType == ScratchpadMemory && numSpmBank > 0) ||
        (nodeType != ScratchpadMemory && numSpmBank == 0),
      s"Number of Scratchpad Bank $numSpmBank and nodeType $nodeType does not make sense"
    )

    // SPM only: the number of bank should make sense
    if (numSpmBank > 0) {
      require(isPow2(numSpmBank), s"Number of Scratchpad Bank should be power of 2")
      require(
        numSpmBank < bandwidth,
        s"Number of bank is more than bandwidth: " +
          s"#bank = $numSpmBank, bandwidth=$bandwidth"
      )
    }

    // DMA and Indirect Scratchpad Only : Must have positive ROB entry number
    if (needROB) {
      require(numPendingRequest > 1 && isPow2(numPendingRequest), s"DMA and Indirect SPM must have ROB")
    }
    if (numPendingRequest > 0) {
      require(needROB || isGEN, s"Memory $this does not need ROB")
    }

    // We only support up to six atomic operation now
    if (AtomicOperations.nonEmpty) {
      require(numAtomOp <= 6, s"We only support up to six atomic operation for now")
    }

    // Only scratchpad memory can support buffet
    if (supportBuffet) {
      require(isSPM, s"Only scratchpad can support buffet")
    }

    // Sanity check passed
    true
  }

  /* ---------- JSON Emitter ---------- */

  // write the memory node parameters to JSON object
  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[MemNodeParameters] {
    def writes(node: MemNodeParameters): JsObject = json.deepMerge(
      Json.obj(
        fields =
          // Node Identification
          "nodeId" -> node.getNodeId,
        "nodeType" -> node.nodeType.toString,
        // Memory Unit Granularity
        "memUnitBits" -> node.memUnitBits,
        "numMemUnitBitsExp" -> node.numMemDataTypeExp,
        // Memory Basic Spec
        "numRead" -> numRead,
        "numWrite" -> numWrite,
        "readWidth" -> readWidth,
        "writeWidth" -> writeWidth,
        "capacity" -> capacity,
        // Memory Meta Info
        "streamStated" -> streamStated,
        // Linear Stream Pattern
        "LinearPadding" -> LinearPadding,
        "MaxLength1D" -> MaxLength1D,
        "MaxAbsStride1D" -> MaxAbsStride1D,
        "MaxLength2D" -> MaxLength2D,
        "MaxAbsStretch2D" -> MaxAbsStretch2D,
        "MaxAbsStride2D" -> MaxAbsStride2D,
        "MaxLength3D" -> MaxLength3D,
        "MaxAbsDeltaStride2D" -> MaxAbsDeltaStride2D,
        "MaxAbsDeltaStretch2D" -> MaxAbsDeltaStretch2D,
        "MaxAbsStretch3D2D" -> MaxAbsStretch3D2D,
        "MaxAbsStretch3D1D" -> MaxAbsStretch3D1D,
        "MaxAbsStride3D" -> MaxAbsStride3D,
        // Indirect Stream Pattern
        "IndirectIndexStream" -> IndirectIndexStream,
        "NumIdxUnitBitsExp" -> NumIdxDataTypeExp,
        "LinearStride2DStream" -> LinearStride2DStream,
        "IndirectStride2DStream" -> IndirectStride2DStream,
        "NumStride2DUnitBitsExp" -> NumStride2DUnitBitsExp,
        "LinearLength1DStream" -> LinearLength1DStream,
        "IndirectLength1DStream" -> IndirectLength1DStream,
        "NumLength1DUnitBitsExp" -> NumLength1DUnitBitsExp,
        // Atomic Update Operation
        "AtomicOperations" -> AtomicOperations.map(_.toString),
        // Buffet
        "supportBuffet" -> supportBuffet,
        // Constant Stream dedicated to Generate Engine
        "numGenDataType" -> numGenDataType,
        // DMA Specific
        "numPendingRequest" -> numPendingRequest,
        // Scratchpad Specific
        "numSpmBank" -> numSpmBank
      )
    )
  }
}

/* ---------- JSON Parser ---------- */
object MemNodeParameters extends JSONParsableConstructor {
  // Default Memory Node Parameter with obvious irrelevant parameters turned off

  // Direct Memory Access, Main Memory Access
  def DMA: MemNodeParameters = MemNodeParameters()
    .copy(numGenDataType = 0, numSpmBank = 0)
    .turnOffLinear3D

  // Scratchpad, optimized for high bandwidth
  def SPM: MemNodeParameters = MemNodeParameters()
    .copy(
      nodeType = ScratchpadMemory,
      readWidth = 32,
      writeWidth = 32,
      capacity = 512 * 1024,
      numSpmBank = 4,
      numGenDataType = 0,
      AtomicOperations = Set.empty
    )
    .turnOffLinear3D

  // Generate Engine, generate numerical sequence automatically
  def GEN: MemNodeParameters = MemNodeParameters()
    .copy(
      nodeType = GenerateEngine,
      numSpmBank = 0,
      numWrite = 0,
      writeWidth = 0,
      AtomicOperations = Set.empty
    )
    .turnOffLinear3D

  // Register Engine, receive scalar value from output vector port
  def REG: MemNodeParameters =
    MemNodeParameters()
      .copy(
        nodeType = RegisterEngine,
        numSpmBank = 0,
        numGenDataType = 0,
        AtomicOperations = Set.empty,
        numPendingRequest = 0
      )
      .turnOffLinear
      .turnOffIndirect

  // Recurrence Engine, send output stream back to input vector port
  def REC: MemNodeParameters =
    MemNodeParameters()
      .copy(
        nodeType = RecurrenceEngine,
        readWidth = 32,
        writeWidth = 32,
        MaxAbsStride1D = 1,
        numSpmBank = 0,
        numGenDataType = 0,
        numPendingRequest = 0,
        AtomicOperations = Set.empty
      )
      .turnOffLinear2D
      .turnOffIndirect

  // parse JSON object to memory node parameters
  def apply(json: JsValue): MemNodeParameters =
    MemNodeParameters(
      // Node Identification
      nodeId = (json \ "nodeId").as[Int],
      nodeType = (json \ "nodeType").as[String],
      // Memory Unit Granularity
      memUnitBits = (json \ "memUnitBits").as[Int],
      numMemDataTypeExp = (json \ "numMemUnitBitsExp").as[Int],
      // Memory Basic Spec
      numRead = (json \ "numRead").as[Int],
      numWrite = (json \ "numWrite").as[Int],
      readWidth = (json \ "readWidth").as[Int],
      writeWidth = (json \ "writeWidth").as[Int],
      capacity = (json \ "capacity").as[Long],
      // Memory Meta Info
      streamStated = (json \ "streamStated").as[Boolean],
      // Linear Stream Pattern
      LinearPadding = (json \ "LinearPadding").as[Boolean],
      MaxLength1D = (json \ "MaxLength1D").as[Int],
      MaxAbsStride1D = (json \ "MaxAbsStride1D").as[Int],
      MaxLength2D = (json \ "MaxLength2D").as[Int],
      MaxAbsStretch2D = (json \ "MaxAbsStretch2D").as[Int],
      MaxAbsStride2D = (json \ "MaxAbsStride2D").as[Int],
      MaxLength3D = (json \ "MaxLength3D").as[Int],
      MaxAbsDeltaStride2D = (json \ "MaxAbsDeltaStride2D").as[Int],
      MaxAbsDeltaStretch2D = (json \ "MaxAbsDeltaStretch2D").as[Int],
      MaxAbsStretch3D2D = (json \ "MaxAbsStretch3D2D").as[Int],
      MaxAbsStretch3D1D = (json \ "MaxAbsStretch3D1D").as[Int],
      MaxAbsStride3D = (json \ "MaxAbsStride3D").as[Int],
      // Indirect Stream Pattern
      IndirectIndexStream = (json \ "IndirectIndexStream").as[Boolean],
      NumIdxDataTypeExp = (json \ "NumIdxUnitBitsExp").as[Int],
      LinearStride2DStream = (json \ "LinearStride2DStream").as[Boolean],
      IndirectStride2DStream = (json \ "IndirectStride2DStream").as[Boolean],
      NumStride2DUnitBitsExp = (json \ "NumStride2DUnitBitsExp").as[Int],
      LinearLength1DStream = (json \ "LinearLength1DStream").as[Boolean],
      IndirectLength1DStream = (json \ "IndirectLength1DStream").as[Boolean],
      NumLength1DUnitBitsExp = (json \ "NumLength1DUnitBitsExp").as[Int],
      // Atomic Update Operations
      AtomicOperations = (json \ "AtomicOperations").as[Seq[String]].map(stringToDsaOperation).toSet,
      // Buffet
      supportBuffet = (json \ "supportBuffet").as[Boolean],
      // Constant Stream Datatype dedicated to Generate Engine
      numGenDataType = (json \ "numGenDataType").as[Int],
      // DMA Specific
      numPendingRequest = (json \ "numPendingRequest").as[Int],
      // SPM Specific
      numSpmBank = (json \ "numSpmBank").as[Int]
    )
}
