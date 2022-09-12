package dsagen2.mem.config

import chisel3.util._

/** The parameters required to describe a scratchpad bank, derived from memory node parameters
  * TODO: This is legacy
  */
case class BankParams(memNode: MemNodeParameters) {

  def num_line: Long = memNode.numRow

  def num_line_bit: Int = memNode.spmBankBitWidth

  def num_line_addr_bit: Int = if (num_line > 1) log2Ceil(num_line) else 0

  def mask_work: Boolean = true

  def bank_size: Int = (memNode.capacity / memNode.numSpmBank).toInt

  def bank_width: Int = memNode.spmBankWidth

  def mem_unit_width: Int = memNode.memUnitBits

  /* Requirement */
  require(memNode.memUnitBits > 0, s"Memory unit width (${memNode.memUnitBits}) should be positive")
  require(bank_size > 0 && isPow2(bank_size), s"Bank size ($bank_size) need to be power of two")
  require(bank_width > 0 && isPow2(bank_width), s"Bank width ($bank_width) need to be power of two")
}
