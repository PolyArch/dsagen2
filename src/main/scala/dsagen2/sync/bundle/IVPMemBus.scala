package dsagen2.sync.bundle

import chisel3._
import dsagen2.mem.bundle.StreamState
import dsagen2.sync.config.IVPNodeParameters
import dsagen2.top.config.DSAFixedConfig.{LINEAR_PADDING_BITS, MAX_LOCAL_VPORT_ID_BIT}

/** The bus that merge all memory read ports from memory node, memory unit width should remain the same, which bus width
  * should be the max of all connected memory engine
  *
  * @param ivpParam    input vector port parameters
  * @param busWidth    The bus width, Max(read width of all memory connected to it)
  * @param busUnitBits The unit bit on the memory bus per unit, usually 8 becuase of byte-addressable memory
  * @param depthBits   The bits to specify the depth of vector port
  * @param padding     Whether this vector port support padding
  * @param stated      Whether this vector port can produce state of stream
  */
class IVPMemBus(
  val ivpParam:    IVPNodeParameters,
  val busWidth:    Int,
  val busUnitBits: Int = 8,
  val depthBits:   Int,
  val padding:     Boolean,
  val stated:      Boolean)
    extends Bundle {

  /* ------------------------- Extract Parameters ------------------------- */
  /* ------------------------- Derived Parameters ------------------------- */
  /* ------------------------- Parameters Sanity  ------------------------- */

  // Make sure width make sense
  require(busWidth <= 64, s"Bus width = $busWidth, Are you sure?")
  require(busUnitBits <= 16, s"Bus unit = $busUnitBits, Are you sure?")

  /* ------------------------- Hardware Fields    ------------------------- */

  /* ----------- Memory <<-- Input Vector Port ----------- */

  // whether the input vector port has space, same as not full
  val ivpReady: Bool = Bool()

  // whether the input vector port consume all data
  // This is useless I think, since we only enqueue when all data can be enqueued
  val ivpReadyMask: UInt = UInt( /*maxWidth*/ 1.W)

  // The number of available entry of input vector port
  val ivpAvailUnits: UInt = UInt(depthBits.W)

  // whether the input vector port do broadcast
  val ivpBroadcast: Option[Bool] = if (ivpParam.broadcastIVP) Some(Bool()) else None

  // The IVP Port ID that this IVP want to broadcast
  // TODO: I don't know how to get node.out.length for each element,
  //  this seems to be a self dependent problem
  //  which means we have to use the MAX bit for worst case
  val ivpBroadcastIVPortId: Option[UInt] =
    if (ivpParam.broadcastIVP) Some(UInt(MAX_LOCAL_VPORT_ID_BIT.W)) else None

  /* ----------- Memory -->> Input Vector Port  ----------- */

  // read request responded
  val memValid: Bool = Bool()

  // read valid for each of data
  val memMask: UInt = UInt(busWidth.W)

  // data and its predication
  val memData: Vec[UInt] = Vec(busWidth, UInt(busUnitBits.W))

  // Connected memory is taking this vector port
  val usedByMem: Bool = Bool()

  // Stream State
  val memStreamState: Option[StreamState] = if (stated) Some(new StreamState) else None

  val memPadding: Option[UInt] = if (padding) Some(UInt(LINEAR_PADDING_BITS.W)) else None

  // reset the broadcast signal when target stream is retired, it should be true when the target stream ends
  val broadcastReset: Option[Bool] = if (ivpParam.broadcastIVP) Some(Bool()) else None

  /* ------------------------- Utility Function   ------------------------- */

}
