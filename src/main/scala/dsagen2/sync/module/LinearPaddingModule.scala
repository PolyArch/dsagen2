package dsagen2.sync.module

import chisel3._
import chisel3.util.{isPow2, log2Ceil}
import dsagen2.mem.bundle.StreamState
import dsagen2.top.config.DSAFixedConfig._

/**
 * Module to calculate the padding vector of data to make the number of element enqueued is multiple of active port * port unit.
 *
 * Legacy: this module should never be used, predication-based lane turning off should never exist
 *
 * @param maxMemWidth The max number of unit connected to this IVP from memory
 * @param vportWidth The vector port width in total, mixed port width supported
 * @param minPortWidth The minimum port width in unit
 * @param queueDepth The depth of vector port in unit
 * @param unitBits The unit bits
 * @param predPad Padding with valid zero but predication off
 * @param zeroPad Padding with valid zero data with predication on
 */
class LinearPaddingModule(maxMemWidth : Int, vportWidth : Int, minPortWidth : Int,
                          queueDepth : Int,
                          unitBits : Int = 8, predPad : Boolean = true, zeroPad : Boolean = true)
  extends MultiIOModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  /* ------------------------- Derived Parameters           ------------------------- */

  // Max number of vector created in padding
  val maxNumVec : Int = maxMemWidth / minPortWidth

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  require(unitBits == 8, s"I think padding is for memory, why it is $unitBits-bit")
  require(isPow2(maxMemWidth), s"Max Memory Width for padding should be power of 2 but $maxMemWidth")
  // TODO: since we have stream state be enabled for the first port, vector port width may not be power of 2
  //  but please check this again
  // require(isPow2(vportWidth), s"Padding Width should be power of 2 but $vportWidth")
  require(isPow2(minPortWidth), s"minimum port width has to be power of ")

  /* ------------------------- Input / Output               ------------------------- */

  // Linear Padding Mode
  val paddingMode : UInt = IO(Input(UInt(LINEAR_PADDING_BITS.W)))

  // State of Stream
  val strState : StreamState = IO(Input(new StreamState))

  // Number of valid element in memory read port bus
  val numValidInRead : UInt = IO(Input(UInt(log2Ceil(maxMemWidth + 1).W)))

  // Number of active element in vector port,
  val numValidInQueue : UInt = IO(Input(UInt(log2Ceil(queueDepth + 1).W)))

  // Number of active port units needed from input vector port (for a 8 x 8B vport, if it is used as 3 wide port,
  // then the active port unit is 24
  val numNeededPerVec : UInt = IO(Input(UInt(log2Ceil(vportWidth + 1).W)))

  // O, Generated Padding Data
  val paddedData : Vec[UInt] = IO(Output(Vec(vportWidth, UInt(unitBits.W))))

  // O, The valid of padding data
  val paddedMask : UInt = IO(Output(UInt(vportWidth.W)))

  // O, The number of vector created in this padding
  val numVecPadded : UInt = IO(Output(UInt(log2Ceil(maxNumVec + 1).W)))

  /* ------------------------- Registers                    ------------------------- */

  /* ------------------------- Modules                      ------------------------- */

  /* ------------------------- Wires                        ------------------------- */

  // C, the number of unit to pad
  val numPadUnit : UInt = WireInit(0.U(log2Ceil(vportWidth + 1).W))
  // 3 byte needed per vector, 5 byte in queue, 6 byte from read
  // in order to pad to multiple of 3, we need to pad to 12 byte, which need 1 byte to pad
  // #pad = #vec - (#queue + #read) % #vec = 3 - (5 + 6) % 3 = 3 - 2 = 1
  numPadUnit := numNeededPerVec - ((numValidInQueue + numValidInRead) % numNeededPerVec)

  // C, whether stream needs padding, not noPadding and padding always happens at stream end/start
  val doPadding : Bool = paddingMode =/= noPad && strState.asUInt().orR()

  // C, whether padding with predicated on
  val doPredOn : Bool = paddingMode === padZeroStrEnd || paddingMode === padZero1DEnd

  // Wire that holds the valid bit of padding result
  val paddedMaskWire : UInt = WireInit(0.U(vportWidth.W))
  paddedMaskWire := (1.U(1.W) << numPadUnit).asUInt() - 1.U

  /* ------------------------- Combination Logic            ------------------------- */

  /* ------------------------- Finite State Machine         ------------------------- */

  /* ------------------------- Output Connection            ------------------------- */

  // Padding Data Value are always zero
  paddedData.foreach(tData => tData := 0.U)

  // Output the padded result
  paddedMask := Mux(doPadding, paddedMaskWire, 0.U)

  // Output the number of vector that this padding will result in
  // The number of vector created by this padding
  // If there are 4 byte in queue, 3 byte needed per vector, 3 byte from read, according above, 2 byte need for pad
  // the first 3 byte in queue already form a vector, the leftover byte are not
  // so the number of vector created in this padding is :
  // 4 (from queue) % 3 (from vector) + 3 (from read) + 2 (from pad) = 1 + 3 + 2 = 6 => 6/3 = 2 vector
  numVecPadded := (numValidInQueue % numNeededPerVec +  numValidInRead + numPadUnit) / numNeededPerVec

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  /* ------------------------- Utility                      ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */
}
