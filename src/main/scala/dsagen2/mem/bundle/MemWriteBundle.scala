package dsagen2.mem.bundle

import chisel3._
import chisel3.util._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.OVP2MemParameter

/** Output Vector Port to Memory Engine Interface
  * Default output direction is from output vector port to memory engine
  * Data Structure:
  *    1. Memory Request: Bool, indicate whether memory is asking for new data
  *       2. Memory Request Mask: Vector of Bool, indicate the location and number of requested data
  *       3. Output Vector Port Response: Bool, indicate whether output vector port has all data prepared
  *       4. Output Vector Port Response Mask: Vector of Bool, delayed Request Mask, indicate whether each position has data
  *       prepared, it can be reset by predication off, like memory engine never request it.
  *       5. Output Vector Port Data: Vector of TagValue, Vector of TagValue that is going to be written into memory
  *       Tag is predication, predication off will reset Response mask
  *       6. Stream State: Stream State that comes with the current response
  *
  * @param ep The output vector port to memory node edge parameter
  */
class MemWriteBundle(val ep: OVP2MemParameter) extends Bundle {

  /* ------------------------- Extract Parameters ------------------------- */

  // Extract the Memory Node Parameter
  val memNode: MemNodeParameters = ep.memNode

  /* ------------------------- Derived Parameters ------------------------- */

  /* ------------------------- Hardware Fields    ------------------------- */

  /* ----------- Output Vector Port <-- Memory ----------- */

  // whether the memory node want data from output vector port, memory Node request data from port to be written
  val memReady: Bool = Input(Bool())

  // Request bit mask whose length is equal to #memUnit
  val memReadyMask: UInt = Input(UInt(memNode.bandwidth.W))

  // Connected memory is taking this vector port
  val usedByMem: Bool = Input(Bool())

  // The data type of stream from memory node, together with Length 1D to support keep enqueue to OVP
  val newMemDataType: Bool = Input(Bool())
  val memDataType: Option[UInt] =
    if (ep.memNode.memDataTypeBits > 0) Some(Input(UInt(ep.memNode.memDataTypeBits.W)))
    else None

  // The length 1D for current 1D stream, enable keep enqueue before memory engine generate real memory request
  // Valid high means there is a new Length1D start
  val memLength1D: Valid[UInt] = Flipped(ValidIO(UInt(ep.memNode.length1DBits.W)))

  /* ----------- Output Vector Port --> Memory  ----------- */

  // The output vector port has data
  val ovpValid: Bool = Output(Bool())

  // OVP bit mask of response data
  val ovpValidMask: UInt = Output(UInt(memNode.bandwidth.W))

  // the actually vector of data unit from the output vector port
  val ovpVecData: Vec[UInt] = Output(Vec(memNode.bandwidth, UInt(memNode.memUnitBits.W)))

  // The number of available byte of the output vector port
  val ovpAvailUnits: UInt = Output(UInt(ep.depthBits.W))

  // Stream State,
  val ovpStreamState: Option[StreamState] =
    if (memNode.streamStated) Some(Output(new StreamState)) else None
  // TODO: the state of stream in output vector port, we should discuss this more luck we have it now
  //    Discussed, Stream Penetration and Stream Association

  /* ------------------------- Utility Function   ------------------------- */
}
