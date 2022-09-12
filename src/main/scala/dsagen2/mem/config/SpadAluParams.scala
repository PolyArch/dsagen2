package dsagen2.mem.config

import chisel3.util.{isPow2, log2Ceil}

case class SpadAluParams(memNode: MemNodeParameters) {
  // Redefined name
  override def toString: String = {
    s"SpadAluParams_d${memNode.spmBankBitWidth}u${memNode.memUnitBits}_" +
      op_name_numExp.map { case (name, exp) => name + exp }.reduce(_ + _)
  }

  // Derived parameters
  def data_width: Int = memNode.spmBankBitWidth

  def unit_width: Int = memNode.memUnitBits

  def max_num_exp: Int = op_name_numExp.map(_._2) max

  def num_exp_bit: Int = if (max_num_exp > 1) log2Ceil(max_num_exp) else 0

  def vec_width: Int = data_width / unit_width

  def num_support_op: Int = op_name_numExp.length

  def max_granularity: Int = unit_width * (1 << (max_num_exp - 1))

  def op_name_numExp: Seq[(String, Int)] = {
    memNode.AtomicOperations.toSeq.map(aop => (aop.toString, memNode.numMemDataTypeExp))
  }

  // require
  op_name_numExp.forall(_._2 > 0)
  require(num_support_op > 0, s"At least one operation needed to be supported")
  require(
    isPow2(vec_width),
    s"Vec width=$data_width/$unit_width = $vec_width, " +
      s"which needs to be power of 2, but now it is $vec_width"
  )
  require(
    max_granularity <= data_width,
    s"Max granularity ($max_granularity) should less than data width ($data_width)"
  )
}
