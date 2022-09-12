package dsagen2.ctrl.bundle.isa

import chisel3._

trait SSISABundle extends Bundle {

  // Hardware Fields that must have
  val function3: UInt
  val opcode:    UInt

  /*  // Sanity check: All SS Command should be standard RISC-V 32 bit command
    require(this.getWidth == 32, s"$this ss isa bundle's bit width is not 32")

    // Sanity check: opcode should be 7 bit
    require(opcode.getWidth == 7, s"Your opcode is not 7 bit but ${opcode.getWidth} bits")

    // Sanity check: function 3 should 3 bit
    require(function3.getWidth == 3, s"Your function3 should be 3 bits not ${function3.getWidth} bit")*/

  /* ---------- Utility ---------- */

  // Get raw bits in UInt
  def rawBits: UInt = {
    val bits: UInt = this.asUInt()
    require(bits.getWidth == 32, s"The bit width of RISC-V instruction should be 32")
    bits
  }

  // Get three-split opcode
  private def opcode3split: (UInt, UInt, UInt) = {
    val op65: UInt = rawBits(6, 5)
    val op42: UInt = rawBits(4, 2)
    val op10: UInt = rawBits(1, 0)
    require(
      op65.getWidth == 2 && op42.getWidth == 3 && op10.getWidth == 2,
      s"The bit width of 3-split opcode should be (2, 3, 2), but now it is " +
        s"(${op65.getWidth}, ${op42.getWidth}, ${op10.getWidth})"
    )
    (op65, op42, op10)
  }

  // Check whether this instruction belongs to custom0 riscv
  def isCustom0: Bool = {
    val (op65, op42, op10) = opcode3split
    op65 === 0.U(2.W) && op42 === 2.U(3.W) && op10 === 3.U(2.W)
  }

  // Check whether this instruction belongs to custom1 riscv
  def isCustom1: Bool = {
    val (op65, op42, op10) = opcode3split
    op65 === 1.U(2.W) && op42 === 2.U(3.W) && op10 === 3.U(2.W)
  }

  // Check whether this instruction belongs to custom2 riscv
  def isCustom2: Bool = {
    val (op65, op42, op10) = opcode3split
    op65 === 2.U(2.W) && op42 === 6.U(3.W) && op10 === 3.U(2.W)
  }

  // Check whether this instruction belongs to custom3 riscv
  def isCustom3: Bool = {
    val (op65, op42, op10) = opcode3split
    op65 === 3.U(2.W) && op42 === 6.U(3.W) && op10 === 3.U(2.W)
  }

  // Check whether this command is vector port configuration
  def isPortCfg: Bool = function3 === 2.U(3.W) && isCustom0

  // Check whether this command is parameter configuration command
  def isParaCfg: Bool = function3 === 3.U(3.W) && isCustom0

  // Check whether this command is linear stream instantiation
  def isLinStrm: Bool = function3 === 2.U(3.W) && isCustom1

  // Check whether this command is linear stream instantiation
  def isIndStrm: Bool = function3 === 2.U(3.W) && isCustom2

  // Check whether this command is linear stream instantiation
  def isRecStrm: Bool = function3 === 2.U(3.W) && isCustom3

  // Check whether this command is receive from vector port command
  def isRecv: Bool = function3 === 6.U(3.W) && isCustom0

  // Check whether this command is synchronization command
  def isWait: Bool = function3 === 6.U(3.W) && isCustom1

  // Check whether this command is performance profile command
  def isStat: Bool = function3 === 6.U(3.W) && isCustom2
}
