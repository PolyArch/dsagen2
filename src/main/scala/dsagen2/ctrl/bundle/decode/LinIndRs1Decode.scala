package dsagen2.ctrl.bundle.decode

import chisel3._

class LinIndRs1Decode extends Bundle {
  /* ---------- Hardware Fields ---------- */

  // Dimension (2-bit) of Linear / Indirect Pattern
  val dimension: UInt = UInt(2.W)

  // Linear Action (1-bit) (0:performance access; 1:linear generation)
  // not used
  val linearAction: UInt = UInt(1.W)

  // Stream Mode (3-bit)
  // Linear: Linear Padding Mode
  // Indirect: Indirect Mode for index/stride2d/length1d
  val streamMode: UInt = UInt(3.W)

  // Memory Type (1-bit) (0:DMA, 1:Scratchpad), this field will be ignored if linearAction is 1 (Generate Engine)
  val memType: UInt = UInt(1.W)

  // Operation Type (3-bit) (0:read, 1:write, 2-7:atomic operation)
  val memOperation: UInt = UInt(3.W)

  // Target Port Local ID
  val targetLocalPortId: UInt = UInt(7.W)

  /* Utility */

  // Whether this stream instantiation instruction is generation stream
  def isGEN: Bool = linearAction === 1.U(1.W)

  // Indirect Index Stream Enabled
  def indirectIndex: Bool = streamMode(0).asBool()

  // Indirect Stride 2D stream
  def indirectStride2D: Bool = streamMode(1).asBool()

  // Indirect Length 1D stream
  def indirectLength1D: Bool = streamMode(2).asBool()

  // Use State of Indirect Index Stream
  def strPenetration: Bool = linearAction.asBool()

  // Stream Association: Stream Length is infinite,
  // will use the state of dependent stream (index, stride2d, length1d) to end
  // If one of the dependent stream reach the end, this stream will also end
  def strAssociation: Bool = dimension(1).asBool()
}
