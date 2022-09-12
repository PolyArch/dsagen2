package dsagen2.mem.module.bank

import chisel3._
import chisel3.util._
import dsagen2.mem.config.SpadAluParams
import dsagen2.top.config.operation.Operation
import dsagen2.top.config.operation.Operation.DsaOperation
import dsagen2.top.dsa.MemBuild
import dsagen2.util.UIntUtil.groupBitsAs

class SPMBankALU(c: SpadAluParams, dsagen: MemBuild) extends MultiIOModule {
  /* ---------- Extract parameters ----------*/

  /* ---------- Input / Output ----------*/
  val exp: Option[UInt] =
    if (c.max_num_exp > 1)
      Some(IO(Input(UInt(c.num_exp_bit.W))))
    else None
  val comp_enable: Bool = IO(Input(Bool()))
  val opcode:      Option[UInt] = Some(IO(Input(UInt(c.memNode.memOperationBits.W))))
  val operand0:    UInt = IO(Input(UInt(c.data_width.W)))
  val operand1:    UInt = IO(Input(UInt(c.data_width.W)))
  val result:      ValidIO[UInt] = IO(ValidIO(UInt(c.data_width.W)))

  /* ---------- Combinational Logics ----------*/

  /* --- Wires --- */
  val op_result_wire: Seq[UInt] =
    Seq.fill(c.num_support_op)(WireDefault(0.U(c.data_width.W)))
  val comp_result: UInt = WireDefault(0.U(c.data_width.W))
    .suggestName("comp_result")

  /* --- Registers --- */
  val result_valid: Bool = RegNext(comp_enable)
  val result_reg:   UInt = RegEnable(comp_result, 0.U, comp_enable)

  /* --- Combinational : Connect each operations result lines --- */
  c.op_name_numExp.zip(op_result_wire).foreach { case ((name, num_exp), result_wire) =>
    decompose_operation(result_wire, num_exp, name)
  }

  /* --- Combinational : Select result between different operation by opcode --- */
  val opcode2result: Seq[(UInt, UInt)] = {
    (c.op_name_numExp zipWithIndex).map { case ((name, _), idx) =>
      val dsaOp:        DsaOperation = Operation.stringToDsaOperation(name)
      val globalEncode: Int = dsagen.fullAtomicOpEnc(dsaOp)
      (globalEncode.U, op_result_wire.apply(idx))
    }
  }

  /* --- Combinational : Get result wire --- */
  if (c.num_support_op > 1) {
    comp_result := Mux(!comp_enable, 0.U, MuxLookup(opcode.get, 0.U, opcode2result))
  } else {
    comp_result := Mux(!comp_enable, 0.U, op_result_wire.head)
  }

  /* ---------- Output Connection ---------- */
  result.valid := result_valid
  result.bits := result_reg

  /* ---------- Utility ---------- */

  def decompose_operation(result_wire: UInt, num_exp: Int, op_name: String): Unit = {
    // Get operation function by name
    val operation_func: Seq[UInt] => Seq[UInt] = hw_operation(op_name)
    // Generate lookup from granularity exp to result
    val exp2result: Seq[(UInt, UInt)] =
      for (exp <- 0 until num_exp) yield {
        // Calculate local data width (granularity) and vec width
        val granularity: Int = c.unit_width * (1 << exp)
        val vec_width:   Int = c.data_width / granularity
        // Group the operand0/1 as granularity width
        val (operand0_gran, operand1_gran): (Vec[UInt], Vec[UInt]) = {
          val vec0: Seq[UInt] = groupBitsAs(operand0, granularity)
          val vec1: Seq[UInt] = groupBitsAs(operand1, granularity)
          require(vec0.length == vec_width && vec1.length == vec_width)
          // rename each sub data
          vec0.zip(vec1).zipWithIndex.foreach { case ((p0, p1), i) =>
            p0.suggestName(s"${op_name}_operand0_g${granularity}_sub$i")
            p1.suggestName(s"${op_name}_operand1_g${granularity}_sub$i")
          }
          // return
          (VecInit(vec0), VecInit(vec1))
        }
        // Calculate the result
        val result_gran: Vec[UInt] = {
          val vec_r: Seq[UInt] =
            (operand0_gran.zip(operand1_gran) zipWithIndex).map { case ((op0, op1), i) =>
              // for now we only support 2->1 hw operation
              operation_func(Seq(op0, op1)).head
                .suggestName(s"${op_name}_result_g${granularity}_sub$i")
                .apply(granularity - 1, 0)
            }
          VecInit(vec_r)
        }
        (
          exp.U,
          result_gran
            .asUInt()
            .suggestName(s"${op_name}_result_exp$exp")
        )
      }
    result_wire := MuxLookup(exp.getOrElse(0.U), 0.U, exp2result)
  }

  /** Some Basic Hardware Operation supported in ALU of Scratchpad Bank, not full set
    * TODO: use the compute function unit to replace the hardware function here
    *
    * @param operationName The name of this function
    * @return A hardware function that map a sequence of UInt to a sequence of UInt
    */
  private def hw_operation(operationName: String): Seq[UInt] => Seq[UInt] = {
    // The provided name should be defined in DsaOperation
    try {
      val operationFound: DsaOperation = dsagen2.top.config.operation.Operation.stringToDsaOperation(operationName)
    } catch {
      case e: Exception =>
        require(requirement = false, s"Operation $operationName is not found")
    }
    val staticMapping = Map(
      // ------ Data Width Insensitive Instructions ------

      // Basic Fixed
      "Add" -> ((ops: Seq[UInt]) => Seq(ops.head + ops(1))),
      "Sub" -> ((ops: Seq[UInt]) => Seq(ops.head - ops(1))),
      "Mul" -> ((ops: Seq[UInt]) => Seq(ops.head * ops(1))),
      "Div" -> ((ops: Seq[UInt]) => Seq(ops.head / ops(1))),
      "Mod" -> ((ops: Seq[UInt]) => Seq(ops.head % ops(1))),
      "Min" -> ((ops: Seq[UInt]) => Seq(ops.head.min(ops(1)))),
      "Max" -> ((ops: Seq[UInt]) => Seq(ops.head.max(ops(1)))),
      // Basic Bitwise
      "BNot" -> ((ops: Seq[UInt]) => Seq((~ops.head).asUInt())),
      "BOr" -> ((ops: Seq[UInt]) => Seq(ops.head | ops(1))),
      "BAnd" -> ((ops: Seq[UInt]) => Seq(ops.head & ops(1))),
      "BXor" -> ((ops: Seq[UInt]) => Seq(ops.head ^ ops(1))),
      // Basic Logical
      "LNot" -> ((ops: Seq[UInt]) => Seq(!ops.head)),
      "LAnd" -> ((ops: Seq[UInt]) => Seq(ops.head =/= 0.U && ops(1) =/= 0.U)),
      "LOr" -> ((ops: Seq[UInt]) => Seq(ops.head =/= 0.U || ops(1) =/= 0.U)),
      "LXor" -> ((ops: Seq[UInt]) => {
        val case1 = ops.head === 0.U && ops(1) =/= 0.U
        val case2 = ops.head =/= 0.U && ops(1) === 0.U
        Seq(case1 || case2)
      }),
      // Basic Compare
      "Comp" -> ((ops: Seq[UInt]) => {
        val iA:  UInt = ops.head
        val iB:  UInt = ops(1)
        val gt:  Bool = iA > iB
        val eq:  Bool = iA === iB
        val lt:  Bool = iA < iB
        val res: UInt = Cat(gt, eq, lt)
        Seq(res)
      })
    )
    staticMapping(operationName)
  }
}
