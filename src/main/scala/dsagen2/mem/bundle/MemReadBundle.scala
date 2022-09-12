package dsagen2.mem.bundle

import chisel3._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.Mem2IVPParameter
import dsagen2.top.config.DSAFixedConfig.{LINEAR_PADDING_BITS, MAX_LOCAL_VPORT_ID_BIT, MAX_VP_BYTE_BITS}

/** Memory Engine to Input Vector Port Interface
  * Default output direction is from memory engine to input vector port
  * Data Structure:
  *    1. Input Vector Port Ready: Bool, indicate whether input vector port receive this vector of data
  *       2. Input Vector Port Ready Mask: Vector of Bool, delayed Valid mask, indicate whether each position's data is
  *       received. Unlike Memory Write Bundle, predicate off cannot reset it.
  *       3. Memory Valid: Bool, indicate whether memory is sending data to input vector port
  *       4. Memory Valid Mask: Vector of Bool, indicate the location and number of sent data
  *       5. Memory Data: Vector of TagValue, Vector of TagValue that is going to be sent to input vector port
  *       Tag is predication, which will be processed by the processing element
  *       6. Stream State: The state of stream that comes with the current request
  *
  * @param ep The edge parameter from memory node to input vector port
  */
class MemReadBundle(val ep: Mem2IVPParameter) extends Bundle {

  /* ------------------------- Extract Parameters ------------------------- */

  // Extract the memory node parameter
  val memNode: MemNodeParameters = ep.memNode

  /* ------------------------- Derived Parameters ------------------------- */

  /* ------------------------- Hardware Fields    ------------------------- */

  /* ----------- Memory <<-- Input Vector Port ----------- */

  // whether the input vector port has space, same as not full
  val ivpReady: Bool = Input(Bool())

  // whether the input vector port consume all data
  // This is useless I think, since we only enqueue when all data can be enqueued
  val ivpReadyMask: UInt = Input(UInt( /*memNode.bandwidth*/ 1.W))

  // The number of available entry of input vector port
  val ivpAvailUnits: UInt = Input(UInt(ep.depthBits.W))

  // whether the input vector port do broadcast
  val ivpBroadcast: Option[Bool] =
    if (ep.ivpNode.broadcastIVP) Some(Input(Bool())) else None

  // The IVP Port ID that this IVP want to broadcast
  // TODO: I don't know how to get node.out.length for each element,
  //  this seems to be a self dependent problem
  //  which means we have to use the MAX bit for worst case
  val ivpBroadcastIVPortId: Option[UInt] =
    if (ep.ivpNode.broadcastIVP) Some(Input(UInt(MAX_LOCAL_VPORT_ID_BIT.W))) else None

  // Input vector port capacity in byte
  val ivpCapa: UInt = Input(UInt(MAX_VP_BYTE_BITS.W))

  // Current byte in input vector port
  val ivpLeftByte: UInt = Input(UInt(MAX_VP_BYTE_BITS.W))

  /* ----------- Memory -->> Input Vector Port  ----------- */

  // read request responded
  val memValid: Bool = Output(Bool())

  // read valid for each of data
  val memValidMask: UInt = Output(UInt(memNode.bandwidth.W))

  // data and its predication
  val memData: Vec[UInt] = Output(Vec(memNode.bandwidth, UInt(memNode.memUnitBits.W)))

  // Connected memory is taking this vector port
  val usedByMem: Bool = Output(Bool())

  // Stream State
  val memStreamState: Option[StreamState] =
    if (memNode.streamStated) Some(Output(new StreamState)) else None

  // Padding Mode
  val memPadding: Option[UInt] =
    if (memNode.LinearPadding) Some(Output(UInt(LINEAR_PADDING_BITS.W))) else None

  // reset the broadcast signal when target stream is retired, it should be true when the target stream ends
  val broadcastReset: Option[Bool] = if (ep.ivpNode.broadcastIVP) Some(Output(Bool())) else None

  // The data type of stream, inform the input vector port what is the current data type
  // TODO: I don't think we need this, since input vector port deal with everything in finest level
  //  but I am not sure, so keep it
  // val memDataType : Option[UInt] = if(ep.dataTypeBits > 0) Some(Output(UInt(ep.dataTypeBits.W))) else None
}

class MemReadPacket(val ep: Mem2IVPParameter) extends Bundle{
  /* ------------------------- Extract Parameters ------------------------- */

  // Extract the memory node parameter
  val memNode: MemNodeParameters = ep.memNode

  /* ----------- Memory -->> Input Vector Port  ----------- */

  // read valid for each of data
  val memValidMask: UInt = UInt(memNode.bandwidth.W)

  // data and its predication
  val memData: Vec[UInt] = Vec(memNode.bandwidth, UInt(memNode.memUnitBits.W))

  // Connected memory is taking this vector port
  val usedByMem: Bool = Bool()

  // Stream State
  val memStreamState: Option[StreamState] = if (memNode.streamStated) Some(new StreamState) else None

  // Padding Mode
  val memPadding: Option[UInt] = if (memNode.LinearPadding) Some(UInt(LINEAR_PADDING_BITS.W)) else None

  // reset the broadcast signal when target stream is retired, it should be true when the target stream ends
  val broadcastReset: Option[Bool] = if (ep.ivpNode.broadcastIVP) Some(Bool()) else None
}
