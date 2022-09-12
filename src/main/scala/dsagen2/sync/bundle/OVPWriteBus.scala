package dsagen2.sync.bundle

import chisel3._
import dsagen2.mem.bundle.StreamState
import dsagen2.sync.config.OVPNodeParameters

class OVPWriteBus(
  val ovpParam:        OVPNodeParameters,
  val busWidth:        Int,
  val busUnitBits:     Int,
  val depthBits:       Int,
  val stated:          Boolean,
  val maxDataTypeBits: Int,
  val maxL1DBits:      Int)
    extends Bundle {
  /* ------------------------- Extract Parameters ------------------------- */
  /* ------------------------- Derived Parameters ------------------------- */

  // Make sure width make sense
  require(busWidth <= 64, s"Bus width = $busWidth, Are you sure?")
  require(busUnitBits <= 16, s"Bus unit = $busUnitBits, Are you sure?")

  /* ------------------------- Hardware Fields    ------------------------- */

  /* ----------- Output Vector Port <<-- Memory ----------- */

  // whether the memory node want data from output vector port, memory Node request data from port to be written
  val memReady: Bool = Bool()

  // Request bit mask whose length is equal to #memUnit
  val memReadyMask: UInt = UInt(busWidth.W)

  // Connected memory is taking this vector port
  val usedByMem: Bool = Bool()

  // The data type of stream from memory node, together with Length 1D to support keep enqueue to OVP
  val newMemDataType: Bool = Bool()
  val memDataType:    Option[UInt] = if (maxDataTypeBits > 0) Some(UInt(maxDataTypeBits.W)) else None

  // The Length 1D for the current 1D stream
  val newMemLength1D: Bool = Bool()
  val memLength1D:    UInt = UInt(maxL1DBits.W)

  /* ----------- Output Vector Port -->> Memory  ----------- */

  // The output vector port has data
  val ovpValid: Bool = Bool()

  // OVP bit mask of response data (can be turn off by predication)
  val ovpValidMask: UInt = UInt(busWidth.W)

  // the actually vector of data unit from the output vector port
  val ovpVecData: Vec[UInt] = Vec(busWidth, UInt(busUnitBits.W))

  // The number of available byte of the output vector port
  val ovpAvailUnits: UInt = UInt(depthBits.W)

  // Stream State,
  // TODO: the state of stream in output vector port, we should discuss this more
  //  Discussed:
  //    Stream Penetration (1 more bit in the indirect stream, indicate whether or not use index stream state)
  //    Stream Association (1 more bit in the )
  val ovpStreamState: Option[StreamState] = if (stated) Some(new StreamState) else None

  /* ------------------------- Utility Function   ------------------------- */
}
