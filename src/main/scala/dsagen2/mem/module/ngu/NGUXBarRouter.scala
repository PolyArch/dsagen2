package dsagen2.mem.module.ngu

import chisel3._
import chisel3.util._

/** A Round-Robin Arbitration for controlling the Offset Crossbar in Stream Number Generation Unit
  *
  * Input:
  *    - Valid: Indicate that there is new request from requestQueue
  *    - ConstExp: The data type exponential of generated stream
  *      Output:
  *    - Offset XBar Routing
  *    - Valid Mask that used for specifying the data type of generated stream
  *
  * @param memBandWidth Memory Bandwidth
  * @param numConstExp  Number of Constant Stream Data Type Exponential
  */
class NGUXBarRouter(memBandWidth: Int, numMemExp: Int, numConstExp: Int) extends MultiIOModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  /* ------------------------- Derived Parameters           ------------------------- */

  // Number of Bits needed for constExp and memoryExp
  def memExpBits: Int = if (numMemExp > 1) log2Ceil(numMemExp) else 0

  def constExpBits: Int = if (numConstExp > 1) log2Ceil(numConstExp) else 0

  // Number of unit per max const data type
  def maxNumUnitPerConst: Int = 1 << (numConstExp - 1)

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // Width and Exp check
  require(
    memBandWidth >> (numConstExp - 1) > 0,
    s"It seems that memory bandwidth is too small to support max exponential = ${numConstExp - 1}"
  )

  /* ------------------------- Input / Output               ------------------------- */

  // Input Valid to indicate that there is new stream request to be converted to generated stream
  val inputValid: Bool = IO(Input(Bool()))

  // Input Mask that specify the mask from Address Generator Unit (ths offset mask)
  val inAddrMask: UInt = IO(Input(UInt(memBandWidth.W)))

  // Output inward ready that indicate whether input vector port is able to receive new generated stream
  // The update of outstanding register can only be done if this signal is true, means IVP accept it
  val outputReady: Bool = IO(Input(Bool()))

  // Input Memory Stream Data Type Exponential
  val mExp: Option[UInt] = if (memExpBits > 0) Some(IO(Input(UInt(memExpBits.W)))) else None

  // Input Constant Stream Data Type Exponential
  val cExp: Option[UInt] = if (constExpBits > 0) Some(IO(Input(UInt(constExpBits.W)))) else None

  // Output Ready, the current mask has been consumed, ready to the upstream to ask for new request
  val inputReady: Bool = IO(Output(Bool()))

  // Output Routing, route the offset to correct place to be added with start point to generate stream
  val offsetRoutes: Seq[UInt] = IO(Output(Vec(memBandWidth, UInt(log2Ceil(memBandWidth + 1).W))))

  // Output Valid Mask, since routing is based on offset unit level, data type should be considered
  val outputValueMask: UInt = IO(Output(UInt(memBandWidth.W)))

  /* ------------------------- Registers                    ------------------------- */

  // Outstanding Mask to keep track of the remaining mask that has not been converted to generated stream
  // Please be attention here, the outstanding mask will never be the same as initial mask from request queue
  // since the initial one will be directly consumed and never be registered into this FF
  val OSAddrMask: UInt = RegInit(0.U(memBandWidth.W))

  // State Register that indicate whether conversion is done based on input or based on outstanding mask
  // Idle: Info from outstanding address mask is useless
  // Busy: All routing info and output should be generated from outstanding address mask, if the current one is the last
  // then it should be set to idle again. Only incoming request that cannot be consumed at the same cycle will trigger
  // this state register and write to outstanding address mask register
  val idle: Bool = RegInit(true.B)

  /* ------------------------- Modules                      ------------------------- */
  /* ------------------------- Wires                        ------------------------- */

  // Const Stream Data Type Exp
  val constExp: UInt = cExp.getOrElse(0.U)

  // Memory Stream Data Type Exp
  val memExp: UInt = mExp.getOrElse(0.U)

  // Internal State that indicate this module is active
  // Conversion from outstanding or convert from input
  val active: Bool = !idle || inputValid

  // Mask out input address mask by memory data type
  // Turn off offset mask bit that is not aligned to memory address
  // Example : [1, 1, 1, 1, 1, 1, 1, 1] => exp = 1 => [0, 1, 0, 1, 0, 1, 0, 1]
  // Example : [1, 1, 1, 1, 0, 0, 0, 0] => exp = 2 => [0, 0, 0, 1, 0, 0, 0, 0]
  // Example : [1, 1, 1, 1, 1, 1, 1, 1] => exp = 3 => [0, 0, 0, 0, 0, 0, 0, 1]
  val inputAddrMask: UInt = {
    val inAddrMaskBools: Seq[Bool] = inAddrMask.asBools()
    val inMaskBools: Seq[Bool] = (0 until memBandWidth).map { unitIdx =>
      // Calculate local unit index in max const data type
      val localUnitIdx: Int = unitIdx % maxNumUnitPerConst
      // Calculate the mask
      if (localUnitIdx == 0)
        inAddrMaskBools(unitIdx) // Lowest position per max data type should always be passed through
      else {
        if (localUnitIdx % 2 == 1) {
          // For odd number, mask can only be passed through when exp is zero
          Mux(memExp === 0.U, inAddrMaskBools(unitIdx), false.B)
        } else {
          // For even number, it can be passed if exp is less or equal with log2(localUnitIdx)
          Mux(memExp <= log2Ceil(localUnitIdx).U, inAddrMaskBools(unitIdx), false.B)
        }
      }
    }
    // Return
    VecInit(inMaskBools).asUInt()
  }

  // Address mask that combine outstanding and input
  val addrMask: UInt = Mux(!idle, OSAddrMask, Mux(inputValid, inputAddrMask, 0.U))

  // The max number of elements can be consumed under current input of exponential
  // If the constant stream exponential == 0 (byte stream, then all should be consumed)
  val numConsumed: UInt = (memBandWidth.U(log2Ceil(memBandWidth + 1).W) >> constExp).asUInt()

  // Convert Address Mask to Bools
  val addrMaskBools: Seq[Bool] = addrMask.asBools()

  // Calculate the valid index + 1 to each position of mask, this is not typical index, the first element whose mask is
  // set has index == 1
  // Example : [0, 1, 0, 0, 1, 1, 0, 0] => [3, 3, 2, 2, 2, 1, 0, 0]
  val validIdx4AddrMaskBools: Seq[UInt] = addrMaskBools
    .map(_.asUInt())
    .scanLeft(0.U) { case (low, high) =>
      low + high // This should create a chain of adder
    }
    .drop(1) // remove the first initial  0
  require(
    validIdx4AddrMaskBools.length == memBandWidth,
    s"Index + 1 for each mask bit has different width with memory band with, " +
      s"Scan left mask bits is ${validIdx4AddrMaskBools.length}, but the given memory bandwidth is $memBandWidth"
  )

  // Combine mask together with number of consumed to mask out the actual place of offset that will be consumed
  require(
    addrMaskBools.length == validIdx4AddrMaskBools.length,
    s"Did you forget to drop the initial one when calculate the index for each mask bit?"
  )
  // This current address mask as bools selects out the bit that will be used for stream generation this cycle
  // The number of true bit should be less or equal to the value specified by [[numConsumed]]
  // Example : [0, 1, 0, 0, 1, 1, 0, 0] => [3, 3, 2, 2, 2, 1, 0, 0] => [0, 0, 0, 0, 1, 1, 0, 0]
  val currAddrMaskBools: Seq[Bool] = addrMaskBools.zip(validIdx4AddrMaskBools).map { case (maskValid, maskBitIndex) =>
    maskValid && maskBitIndex <= numConsumed
  }
  require(currAddrMaskBools.length == memBandWidth)
  val currAddrMask: UInt = VecInit(currAddrMaskBools).asUInt()

  // Tag each mask bit with select value starting with 1, since zero select ground
  val maskSels: Seq[UInt] = (1 to memBandWidth).map(x => x.U(log2Ceil(memBandWidth + 1).W))

  // Calculate index of destination unit for each valid mask bit in current address mask (constrained by number of consumed)
  // Please be attention: this index of destination unit starts from zero, since it need to be combined with exp
  require(currAddrMaskBools.length == validIdx4AddrMaskBools.length)
  // Example : [3, 3, 2, 2, 2, 1, 0, 0] => [0, 0, 0, 0, 1, 1, 0, 0] => [0, 0, 0, 0, 1 << exp, 0 << exp, 0, 0]
  val destPerCurrAddrMaskBit: Seq[UInt] =
    currAddrMaskBools.zip(validIdx4AddrMaskBools).map { case (currMaskBit, index) =>
      // Since the index from [[validIdx4AddrMaskBools]] start from 1, in order to calculate the correct index, we should
      // -1 before scaling to correct exponential
      Mux(currMaskBit, ((index - 1.U) << constExp).asUInt(), 0.U)
    }

  // Calculate OneHot selection for each output port routing
  require(currAddrMaskBools.length == destPerCurrAddrMaskBit.length)
  require(currAddrMaskBools.length == memBandWidth)
  val offsetRouteOH: Seq[UInt] = (0 until memBandWidth).map { outIdx =>
    val onehot: Seq[Bool] = currAddrMaskBools.zip(destPerCurrAddrMaskBit).map { case (currValid, destIdx) =>
      currValid && destIdx === outIdx.U
    }
    VecInit(onehot).asUInt()
  }

  // Check whether each unit position has source
  val offsetRouteHasSource: Seq[Bool] = offsetRouteOH.map(_.orR())

  // The current address mask is equal to the address mask combined
  require(addrMask.getWidth == currAddrMaskBools.length)
  val lastRequest: Bool = addrMask === currAddrMask

  // Calculate the next cycle address mask (bits turn off by current mask bits that are true)
  val nextAddrMask: UInt = addrMask & (~currAddrMask).asUInt()

  // Write to outstanding address mask register if the incoming request cannot be consumed in one cycle
  val newWrite2Reg: Bool = {
    // Requirement: The state register is idle, input valid is true
    val newRequest: Bool = idle && inputValid
    // A new request that cannot be consumed in this cycle, should be written into outstanding register
    // The address mask to be written into register also need to consider that whether downstream accepts the current one
    !lastRequest && newRequest
  }

  /* ------------------------- Combination Logic            ------------------------- */

  // Calculate the output mask combined with exponential

  import dsagen2.misc.util.ReduceUtil.extendMaskByExp

  val valueMask: UInt = extendMaskByExp(VecInit(offsetRouteHasSource).asUInt(), cExp, numConstExp)
  require(
    valueMask.getWidth == memBandWidth,
    s"Value mask should be $memBandWidth-bit, but it is ${valueMask.getWidth}"
  )

  /* ------------------------- Finite State Machine         ------------------------- */

  // FSM : Turn on/off the state register off outstanding address mask register
  when(newWrite2Reg) {
    // Kick start the state register to perform RR Arbitration based on outstanding address mask register instead on input
    idle := false.B
  }.elsewhen(!idle && lastRequest && outputReady) {
    // If the last request is accepted and it is the last request, turn off the FSM
    idle := true.B
  }

  // FSM: Update the outstanding address mask register based on [[writeToReg]] and outputReady
  when(newWrite2Reg) {
    // If new write to outstanding register is accepted by IVP, then write the next one, otherwise write the current one
    OSAddrMask := Mux(outputReady, nextAddrMask, inputAddrMask)
  }.elsewhen(!idle) {
    // Outstanding Mask Register Updated only when output ready and the end of current request
    when(outputReady) {
      OSAddrMask := Mux(lastRequest, 0.U, nextAddrMask)
    }
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Connect to the offset crossbar routing
  require(offsetRouteOH.length == offsetRoutes.length)
  offsetRoutes.zip(offsetRouteOH).foreach { case (route, onehot) => route := Mux1H(onehot, maskSels) }

  // Input Ready should be combined with output ready and the last request
  inputReady := outputReady && lastRequest

  // Output to value mask
  outputValueMask := valueMask

  /* ------------------------- Hardware Sanity Check        ------------------------- */
  /* ------------------------- Post Generation Sanity Check ------------------------- */
  /* ------------------------- Utility                      ------------------------- */
}
