package dsagen2.ctrl.bundle.isa

import chisel3._
import dsagen2.top.config.DSAFixedConfig.MAX_LOCAL_VPORT_ID_BIT

/** Parsed bundle of Receive command
  */
class SSRecv extends SSISABundle {

  /* ---------- Hardware Field ----------*/

  // Specifying which output vector port we receive value from
  // inst[31:25]
  val targetLocalPortId: UInt = UInt(MAX_LOCAL_VPORT_ID_BIT.W)
  require(targetLocalPortId.getWidth == 7, s"Index of output vector port should be 7 bit")

  // Two bits field that is not used for now
  // inst[24:22]
  val EMPTY: UInt = UInt(3.W)

  // 0: discard Length1D element;
  // 1: send one XLEN-bit value from ovp to CPU's rd;
  // 2. send one XLEN-bit value from CPU's rs1 to ivp;
  // inst[21:20]
  val commMode: UInt = UInt(2.W)

  // CPU rs1 : providing the register value that going to be sent to input vector port when commMode = cpu2ivp
  // inst[19:15]
  val cpuRs1: UInt = UInt(5.W)

  // Function 3-bit, not used in SSRecv command
  // inst[14:12]
  val function3: UInt = UInt(3.W)

  // CPU rd, the CPU destination register for receive stream
  // inst[11:7]
  val cpuRd: UInt = UInt(5.W)

  // Opcode
  // inst[6:0]
  val opcode: UInt = UInt(7.W)

  /* ---------- Utility ----------*/

  // Whether is discarding the data from port
  def isDiscard: Bool = commMode === 0.U(2.W)

  def ovp2cpu: Bool = commMode === 1.U(2.W)

  def cpu2ivp: Bool = commMode === 2.U(2.W)

  def isREG: Bool = ovp2cpu || cpu2ivp
}
