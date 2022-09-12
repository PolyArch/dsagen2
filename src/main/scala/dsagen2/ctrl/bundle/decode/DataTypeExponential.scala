package dsagen2.ctrl.bundle.decode

import chisel3._

/** Data Type Exponential Bundle used to parse DSA Register
  */
class DataTypeExponential extends Bundle {
  // Data Type of Indirect Length 1D Stream from Output Vector Port
  val length1DStrDataTypeExp: UInt = UInt(2.W)

  // Data Type of Indirect Stride 2D Stream from Output Vector Port
  val stride2DStrDataTypeExp: UInt = UInt(2.W)

  // Data Type of Indirect Index Stream from Output Vector Port
  val indexStrDataTypeExp: UInt = UInt(2.W)

  // Data Type of Linear Generated Stream
  val constStrDataTypeExp: UInt = UInt(2.W)

  // Data Type of Memory Stream
  val memUnitWidthExp: UInt = UInt(2.W)
}
