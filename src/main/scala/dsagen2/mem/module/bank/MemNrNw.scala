package dsagen2.mem.module.bank

import chisel3._
import chisel3.util._
import dsagen2.mem.config.BankParams
import dsagen2.util.UIntUtil.groupBitsAs

class MemNrNw(numRead: Int, numWrite: Int, p: BankParams) extends MultiIOModule {
  // Rename
  suggestName(
    s"SyncReadMem_" +
      s"${numRead}r${numWrite}w_s${p.bank_size}u${p.mem_unit_width}w${p.bank_width}l${p.num_line}"
  )

  /*---------- Input / Output ----------*/

  /*----- Read -----*/
  // Input
  val vec_read_enable: Vec[Bool] = IO(Input(Vec(numRead, Bool())))
  val vec_read_addr: Option[Vec[UInt]] =
    if (p.num_line_addr_bit > 0) Some(IO(Input(Vec(numRead, UInt(p.num_line_addr_bit.W))))) else None
  // Output
  val vec_read_data: Vec[ValidIO[UInt]] = IO(Vec(numRead, ValidIO(UInt(p.num_line_bit.W))))

  /*------ Write -----*/
  // Input
  val vec_write_enable: Vec[Bool] = IO(Input(Vec(numWrite, Bool())))
  val vec_write_addr: Option[Vec[UInt]] =
    if (p.num_line_addr_bit > 0) Some(IO(Input(Vec(numWrite, UInt(p.num_line_addr_bit.W))))) else None
  val vec_write_data: Vec[UInt] = IO(Input(Vec(numWrite, UInt(p.num_line_bit.W))))
  val vec_write_mask: Option[Vec[UInt]] =
    if (p.mask_work) Some(IO(Input(Vec(numWrite, UInt(p.bank_width.W))))) else None

  /* ---------- Combinational Logics ---------- */
  // Vec of 0.U as #read
  val vec_r_zero_addr: Vec[UInt] = VecInit(Seq.fill(numRead)(0.U))
  val vec_w_zero_addr: Vec[UInt] = VecInit(Seq.fill(numWrite)(0.U))
  val vec_w_true_mask: Vec[UInt] =
    VecInit(Seq.fill(numWrite)(Fill(p.bank_width, 1.U(1.W))))
  // Memory (write first memory)
  val mem: SyncReadMem[Vec[UInt]] = SyncReadMem(p.num_line, Vec(p.bank_width, UInt(p.mem_unit_width.W)))

  /*----- Combinational : Read -----*/
  (vec_read_enable.zip(vec_read_data) zipWithIndex).foreach { case ((read_enable, read_data), idx) =>
    val read_addr: UInt =
      vec_read_addr.getOrElse(vec_r_zero_addr).apply(idx)
    read_data.valid := RegNext(read_enable)
    read_data.bits := mem.read(read_addr, read_enable).asUInt()
  }

  /*----- Combinational : Write -----*/
  (vec_write_enable.zip(vec_write_data) zipWithIndex).foreach { case ((write_enable, write_data), idx) =>
    // Extract Addr and Mask
    val write_addr: UInt =
      vec_write_addr.getOrElse(vec_w_zero_addr).apply(idx)
    val write_mask: UInt =
      vec_write_mask.getOrElse(vec_w_true_mask).apply(idx)
    val write_vec_data: Vec[UInt] =
      VecInit(groupBitsAs(write_data, p.mem_unit_width))
    // Reconstruct write mask to prioritize lower write
    val lower_prioritized_mask: UInt = {
      if (idx == 0) write_mask // lowest port has the highest priority
      else {
        // Lower port not enabled or addr different
        val lower_noEnabled_OR_addrDiff: Seq[Bool] = {
          for (low_idx <- 0 until idx) yield {
            val low_enable: Bool = vec_write_enable(low_idx)
              .suggestName(s"w${idx}_enabled")
            val low_addr: UInt =
              vec_write_addr.getOrElse(vec_w_zero_addr).apply(low_idx)
            val no_conflict: Bool =
              (!low_enable).suggestName(s"w${low_idx}_not_enabled") ||
                (low_addr =/= write_addr)
                  .suggestName(s"addr_w${low_idx}_neq_w$idx")
            no_conflict.suggestName(s"no_conflict_w${low_idx}Xw$idx")
            no_conflict
          }
        }
        // Reconstruct the mask
        val prioritized_mask: Vec[Bool] = {
          val mask: Seq[Bool] =
            for (width_idx <- 0 until p.bank_width) yield {
              // detect lower conflict for same width position
              val low_mask_no_conflict: Seq[Bool] =
                for (low_idx <- 0 until idx) yield {
                  // Either lower port is not enabled
                  // Or the same width location mask is not true
                  lower_noEnabled_OR_addrDiff(low_idx)
                    .suggestName(s"w${low_idx}_nE_OR_aDiff_w$idx") ||
                  (!vec_write_mask
                    .getOrElse(vec_w_true_mask)
                    .apply(low_idx)
                    .asBools()
                    .apply(width_idx))
                    .suggestName(s"not_w${low_idx}m$width_idx")
                }
              // Lower mask must be conflict free AND current mask is set
              VecInit(low_mask_no_conflict)
                .asUInt()
                .andR()
                .suggestName(s"low_w${idx}_mask_no_conflict") &&
              write_mask
                .asBools()
                .apply(width_idx)
                .suggestName(s"w${idx}m$width_idx")
            }
          VecInit(mask)
        }
        prioritized_mask.asUInt()
      }
    }
    // Write
    when(write_enable) {
      mem.write(write_addr, write_vec_data, lower_prioritized_mask.asBools())
    }
  }
}
