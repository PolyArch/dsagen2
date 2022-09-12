package dsagen2.ctrl.config

/** The different type of Decoupled-Spatial Register
  */
object DSARegister extends Enumeration {
  // Specify the type of Register Type Enumeration
  type DSARegister = Value

  // Always Zero Register
  val dsaRegZero: DSARegister = Value

  // To Be Configured Register
  // TODO: for now we don't have a concrete plan for multi-accelerator per core, so this register is not in used
  val dsaRegTBC: DSARegister = Value

  // Configuration Bitstream Starting Address
  val dsaRegCSA: DSARegister = Value

  // Configuration Bitstream size (in byte)
  val dsaRegCFS: DSARegister = Value

  // Exponential Register, which encodes the data type
  val dsaRegExp: DSARegister = Value

  // Stream Base Parameter: Starting Address
  val dsaRegStreamStartAddr: DSARegister = Value

  // Stream Base Parameter: Stride 1D
  val dsaRegStreamStride1D: DSARegister = Value

  // Stream Base Parameter: Length 1D
  val dsaRegStreamLength1D: DSARegister = Value

  // Stream Linear 2D Parameter: Stretch 2D
  val dsaRegStreamStretch2D: DSARegister = Value

  // Stream Linear 2D Parameter: Stride 2D
  val dsaRegStreamStride2D: DSARegister = Value

  // Stream Linear 2D Parameter: Length 2D
  val dsaRegStreamLength2D: DSARegister = Value

  // Stream Linear 3D Parameter: Delta Stretch 2D
  val dsaRegStreamDeltaStretch2D: DSARegister = Value

  // Stream Linear 3D Parameter: Delta Stride 2D
  val dsaRegStreamDeltaStride2D: DSARegister = Value

  // Stream Linear 3D Parameter: Stretch 3D to 1D
  val dsaRegStreamStretch3D1D: DSARegister = Value

  // Stream Linear 3D Parameter: Stretch 3D to 2D
  val dsaRegStreamStretch3D2D: DSARegister = Value

  // Stream Linear 3D Parameter: Stride 3D
  val dsaRegStreamStride3D: DSARegister = Value

  // Stream Linear 3D Parameter: Length 3D
  val dsaRegStreamLength3D: DSARegister = Value

  // Indirect Ports Register, which encodes the indirect port
  val dsaRegIndirectPorts: DSARegister = Value

  // Buffet Spec Register, which encodes the start and end address of buffet
  val dsaRegBuffetAddr: DSARegister = Value

  // Buffet State Register, which encodes:
  // IsBuffet, BuffetIdx, BuffetStreamType, HasUpdateStream, HasShrinkStream, UpdateStreamPort, ShrinkStreamPort
  val dsaRegBuffetState: DSARegister = Value

  // Maximum number of register
  def maxNumDSARegister: Int = values.size
}
