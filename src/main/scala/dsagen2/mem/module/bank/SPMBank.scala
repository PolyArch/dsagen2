package dsagen2.mem.module.bank

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.mem.bundle.{MemRequest, MemResponse}
import dsagen2.mem.config.{BankParams, MemNodeParameters, SpadAluParams}
import dsagen2.top.config.DSAFixedConfig.{memOpRead, memOpWrite}
import dsagen2.top.dsa.MemBuild
import dsagen2.util.UIntUtil.{allOneUInt, groupBitsAs}

class SPMBank(memNode: MemNodeParameters, dsagen: MemBuild)(implicit val p: Parameters) extends MultiIOModule {

  /* -------------------------      Extract Parameters      ------------------------- */

  /* -------------------------     Derived Parameters       ------------------------- */

  // Whether or not support Atomic Operation
  def supportAtomOp: Boolean = memNode.supportAtomicOperation

  // Number of Atomic Operation Data Type
  def numAtomDataTypeExp: Int = if (supportAtomOp) memNode.numMemDataTypeExp else 0

  /* -------------------------      Parameters Sanity Check ------------------------- */

  /* -------------------------         Input / Output       ------------------------- */

  // Request Port
  val memRequest: MemRequest = IO(Input(new MemRequest(memNode)))

  // Response Port
  val memResponse: MemResponse = IO(Output(new MemResponse(memNode)))

  /* -------------------------     Registers and Modules    ------------------------- */

  /* read out register */

  // record read out address
  val readAddrReg: Option[UInt] = if (memNode.hasStrRequest) Some(RegInit(0.U(memNode.addrBits.W))) else None

  /* normal write out register (overwrite COW) */

  // record write out valid
  val write_enable_reg: Bool = RegInit(false.B)

  // record write out addr
  val write_addr_reg: Option[UInt] = if (memNode.hasStrRequest) Some(RegInit(0.U(memNode.addrBits.W))) else None

  // record normal write out data
  val write_data_reg: UInt = RegInit(0.U(memNode.spmBankBitWidth.W))

  // record write out mask (All one because first write can always update all)
  val write_mask_reg: Option[UInt] = Some(RegInit(Fill(memNode.spmBankWidth, 1.U(1.W))))

  /* atomic update (read out/comp in) register (read out valid, operands, exp) */
  val aupd_read_out_exp_reg: Option[UInt] =
    if (supportAtomOp && numAtomDataTypeExp > 1) Some(RegInit(0.U(log2Ceil(numAtomDataTypeExp).W))) else None
  val aupd_read_out_opcode_reg: Option[UInt] =
    if (supportAtomOp) Some(RegInit(0.U(memNode.memOperationBits.W))) else None
  val aupd_read_out_valid_reg: Option[Bool] =
    if (supportAtomOp) Some(RegInit(false.B)) else None
  val aupd_comp_operand1_reg: Option[UInt] =
    if (supportAtomOp) Some(RegInit(0.U(memNode.spmBankBitWidth.W))) else None
  val aupd_read_out_mask_reg: Option[UInt] = Some(RegInit(allOneUInt(memNode.spmBankWidth)))

  /* atomic update (comp out/write in) register */
  val aupd_write_addr_reg: Option[UInt] =
    if (memNode.hasStrRequest) Some(RegInit(0.U(memNode.addrBits.W)))
    else None
  val aupd_write_mask_reg: Option[UInt] = Some(RegInit(allOneUInt(memNode.spmBankWidth)))

  /* -------------------------             Wires            ------------------------- */

  // normal read
  val read_enable: Bool = WireDefault(false.B)
  val read_addr: Option[UInt] =
    if (memNode.hasStrRequest) Some(WireDefault(0.U(memNode.addrBits.W))) else None
  val read_data_valid: Bool = WireDefault(false.B)
  val read_data:       UInt = WireDefault(0.U(memNode.spmBankBitWidth.W))
  // normal write
  val write_enable: Bool = WireDefault(false.B)
  val write_data:   UInt = WireDefault(0.U(memNode.spmBankBitWidth.W))
  val write_addr: Option[UInt] =
    if (memNode.hasStrRequest) Some(WireDefault(0.U(memNode.addrBits.W))) else None
  val write_mask: Option[UInt] = Some(WireDefault(Fill(memNode.spmBankWidth, 1.U(1.W))))
  // atomic update read input (read input)
  val aupd_read_enable: Option[Bool] =
    if (supportAtomOp) Some(WireDefault(false.B)) else None
  // atomic update compute input (read output)
  val aupd_comp_enable: Option[Bool] =
    if (supportAtomOp) Some(WireDefault(false.B)) else None
  val aupd_comp_addr: Option[UInt] =
    if (supportAtomOp && memNode.hasStrRequest)
      Some(WireDefault(0.U(memNode.addrBits.W)))
    else None
  val aupd_comp_operand0: Option[UInt] =
    if (supportAtomOp) Some(WireDefault(0.U(memNode.spmBankBitWidth.W)))
    else None
  // atomic update write input (compute output)
  val aupd_comp_result: Option[UInt] =
    if (supportAtomOp) Some(WireDefault(0.U(memNode.spmBankBitWidth.W))) else None
  val aupd_write_enable: Option[Bool] =
    if (supportAtomOp) Some(WireDefault(false.B)) else None
  val aupd_write_addr: Option[UInt] =
    if (supportAtomOp) Some(WireDefault(0.U(memNode.addrBits.W))) else None
  val aupd_write_data: Option[UInt] =
    if (supportAtomOp) Some(WireDefault(0.U(memNode.spmBankBitWidth.W))) else None
  val aupd_write_mask: Option[UInt] =
    if (supportAtomOp) Some(WireDefault( /*Fill(memNode.spmBankWidth, 1.U(1.W))*/ allOneUInt(memNode.spmBankWidth)))
    else None

  /* -------------------------     Combinational Logics     ------------------------- */

  /** We need a 1 read port and 2 write ports memory, because atomic update will
    * compete the write port with normal write request.
    * Here are somes pipeline cases for you to think of:
    * case 1:
    * AR   | ARO | COW | AWO |
    * | R   | RO  |     | // Forwarding COW -> RO
    * | R   | RO  |
    * case 2:
    * AR  | ARO | COW | AWO |
    * | W   | WO  |     | // Back forwarding WO -> COW
    * | W   | WO  |
    * case 3:
    * AR  | ARO | COW | AWO |
    * | AR  | ARO | COW | AWO | // Forwarding COW -> ARO (same as case 1)
    * |     | AR  | ARO | COW | AWO |
    * Annotations:
    * AR:   atomic update send read request
    * ARO:  atomic update read out the data from memory
    * COW:  atomic update get the result and send write request to memory
    * AWO:  atomic update finished writing to memory
    * R: normal read send read request to memory
    * RO: normal read get the data out from memory
    * W: normal write send write request to memory
    * WO: normal write finished wrirting to memory
    */
  // Memory Block
  val num_write_port: Int = 1 + {
    if (supportAtomOp) 1 else 0
  }
  val mem: MemNrNw = Module(new MemNrNw(1, num_write_port, BankParams(memNode)))

  // Extract the read-related port since we only have one read port
  val read_port_enable: Bool = mem.vec_read_enable.head
  val read_port_addr: Option[UInt] =
    mem.vec_read_addr match {
      case Some(addr_port) => Some(addr_port.head)
      case None            => None
    }
  val read_port_data:       UInt = mem.vec_read_data.head.bits
  val read_port_data_valid: Bool = mem.vec_read_data.head.valid
  // Extract the write-related port from lowest write port
  val write_port_enable: Bool = mem.vec_write_enable.head
  val write_port_addr: Option[UInt] =
    mem.vec_write_addr match {
      case Some(addr_port) => Some(addr_port.head)
      case _               => None
    }
  val write_port_data: UInt = mem.vec_write_data.head
  val write_port_mask: Option[UInt] = mem.vec_write_mask match {
    case Some(mask) => Some(mask.head)
    case None       => None
  }
  // Extract the atomic update-related port from second write port
  val aupd_port_write_enable: Option[Bool] =
    if (supportAtomOp) Some(mem.vec_write_enable.apply(1)) else None
  val aupd_port_write_addr: Option[UInt] =
    (supportAtomOp, mem.vec_write_addr) match {
      case (true, Some(addr_port)) => Some(addr_port.apply(1))
      case _                       => None
    }
  val aupd_port_write_data: Option[UInt] =
    if (supportAtomOp) Some(mem.vec_write_data.apply(1)) else None
  val aupd_port_write_mask: Option[UInt] =
    (supportAtomOp, mem.vec_write_mask) match {
      case (true, Some(mask)) => Some(mask.apply(1))
      case _                  => None
    }
  // Atomic Update ALU Block
  val alu: Option[SPMBankALU] =
    if (supportAtomOp) Some(Module(new SPMBankALU(SpadAluParams(memNode), dsagen)))
    else None

  /* --- Combinational : Connect Request to Read Port ---*/

  // Connect Request to Internal Wire
  read_enable := memRequest.valid && memRequest.memOp === memOpRead
  read_addr match {
    case Some(value) => value := memRequest.vaddr
    case None        =>
  }
  readAddrReg match {
    case Some(value) => value := read_addr.getOrElse(0.U)
    case None        =>
  }

  // Connect Read Input Wire to Read Input Port
  read_port_enable := aupd_read_enable.getOrElse(false.B) || read_enable
  read_port_addr match {
    case Some(value) =>
      require(read_addr.isDefined)
      // Assign higher row address
      value := read_addr.get.apply(read_addr.get.getWidth - 1, memNode.lowerPosRowAddr)
    case None =>
  }

  // Connect Read Out Port to Read Out Wire
  read_data_valid := read_port_data_valid

  /* --- Combinational : Connect Request to Write Port ---*/

  // Connect Request to Internal Wire
  write_enable := !aupd_read_enable.getOrElse(false.B) && memRequest.valid && memRequest.memOp === memOpWrite
  write_addr match {
    case Some(value) => value := memRequest.vaddr
    case None        =>
  }
  require(write_data.getWidth == memRequest.data.getWidth)
  write_data := memRequest.data
  write_mask match {
    case Some(value) => value := memRequest.mask
    case None        =>
  }

  // Connect Internal Wire to Write Port
  write_port_enable := write_enable
  write_port_addr match {
    case Some(value) =>
      require(write_addr.isDefined)
      // Assign higher row address
      value := write_addr.get.apply(write_addr.get.getWidth - 1, memNode.lowerPosRowAddr)
    case None =>
  }
  require(write_port_data.getWidth == write_data.getWidth)
  write_port_data := write_data
  write_port_mask match {
    case Some(value) => value := write_mask.getOrElse(0.U)
    case None        =>
  }

  // Connect the write request to Register
  write_enable_reg := write_enable
  when(write_enable) { // This is actually RegEnable, not FSM
    write_addr_reg match {
      case Some(value) => value := write_addr.getOrElse(0.U)
      case None        =>
    }
    write_data_reg := write_data
    write_mask_reg match {
      case Some(value) => value := write_mask.getOrElse(0.U)
      case None        =>
    }
  }

  /* --- Combinational : Connect Request to Atomic Update Related Wire */
  // Connect Request to Internal Wire
  option_:=(aupd_read_enable, Some(memRequest.valid && memRequest.isAtomOp))
  // Record atomic update read out valid
  option_:=(aupd_read_out_valid_reg, aupd_read_enable)
  // Connect atomic update read out valid && read data valid to compute enable
  option_:=(aupd_comp_enable, Some(aupd_read_out_valid_reg.getOrElse(false.B) && read_data_valid))

  /* --- Combinational : Passing atomic update value to register --- */
  option_enable_:=(aupd_read_out_exp_reg, aupd_read_enable, memRequest.memDataTypeExp)
  option_enable_:=(aupd_read_out_opcode_reg, aupd_read_enable, Some(memRequest.memOp))
  option_enable_:=(aupd_comp_operand1_reg, aupd_read_enable, Some(memRequest.data))
  (aupd_read_out_mask_reg, aupd_read_enable, Some(memRequest.mask)) match {
    case (Some(mask_reg), Some(b), Some(mask)) =>
      when(b) {
        mask_reg := mask
      }
    case _ =>
  }

  /* --- Combinational : Passing Read Out Addr and Mask to Write in register ---*/
  option_:=(aupd_write_addr_reg, readAddrReg)
  option_:=(aupd_write_mask_reg, aupd_read_out_mask_reg)

  /* --- Combinational : Connect Atomic Update related wire to ALU ---*/
  alu match {
    case Some(a) =>
      // alu input
      option_:=(a.exp, aupd_read_out_exp_reg) // connect exp
      a.comp_enable := aupd_comp_enable.get // connect enable
      option_:=(a.opcode, aupd_read_out_opcode_reg) // connect opcode
      // TODO: switch the order of operand0/1 in future
      option_:=(Some(a.operand0), Some(read_data))
      option_:=(Some(a.operand1), aupd_comp_operand1_reg)
      // alu output
      option_:=(aupd_comp_result, Some(a.result.bits))
      option_:=(aupd_write_enable, Some(a.result.valid))
    case _ =>
  }

  /* --- Combinational : Connect Atomic Update Output to Write in Port---*/
  option_:=(aupd_port_write_enable, aupd_write_enable)
  aupd_port_write_addr match {
    case Some(value) =>
      require(aupd_write_addr_reg.isDefined)
      value := aupd_write_addr_reg.get.apply(aupd_write_addr_reg.get.getWidth - 1, memNode.lowerPosRowAddr)
    case None =>
  }
  option_:=(aupd_port_write_data, aupd_write_data)
  option_:=(aupd_write_mask, aupd_write_mask_reg)
  option_:=(aupd_port_write_mask, aupd_write_mask)

  /* --- Combinational : Forwarding  ---*/
  if (!supportAtomOp) {
    read_data := read_port_data
  } else {
    // check if computed out address is equal to read out addr
    val addr_ro_eq_cor: Bool = {
      read_data_valid && aupd_write_enable.getOrElse(false.B) &&
      readAddrReg.getOrElse(0.U) === aupd_write_addr_reg.getOrElse(0.U)
    }
    // Get the write out addr to see if it is equal to aup write addr
    val addr_cow_eq_wo: Bool = {
      write_enable_reg && aupd_write_enable.get &&
      write_addr_reg.getOrElse(0.U) === aupd_write_addr_reg.getOrElse(0.U)
    }
    // Forwarding with mask
    (aupd_write_mask_reg, write_mask_reg) match {
      case (Some(aupd_mask), Some(write_mask)) =>
        // split write data and computed data into bank width sequence
        val vec_aupd_write_data: Seq[UInt] =
          groupBitsAs(aupd_write_data.get, memNode.memUnitBits)
        val vec_comp_out_data: Seq[UInt] =
          groupBitsAs(aupd_comp_result.get, memNode.memUnitBits)

        // Atomic Update -> Read Out (case 1/3)
        val vec_read_out_data: Seq[UInt] = groupBitsAs(read_port_data, memNode.memUnitBits)
        // Form forwarded data
        val vec_forward_data: Seq[UInt] =
          (vec_aupd_write_data
            .zip(vec_read_out_data)
            .zip(
              // for some reason, chisel3 cannot recognize the width here
              aupd_mask.apply(memNode.spmBankWidth - 1, 0).asBools()
            ) zipWithIndex).map { case (((cow, ro), m), idx) =>
              Mux(
                option_&&(Some(addr_ro_eq_cor), m),
                cow.suggestName(s"comp_out_write_sub$idx"),
                ro.suggestName(s"read_out_sub$idx")
              )
            }
        // Assign to read out
        read_data := VecInit(vec_forward_data).asUInt()

        // Write Out -> Atomic Update (case 2)
        val vec_write_out_data: Seq[UInt] = groupBitsAs(write_data_reg, memNode.memUnitBits)
        // For back forwarded data
        val vec_back_forward_data: Seq[UInt] =
          (vec_comp_out_data
            .zip(vec_write_out_data)
            .zip(
              // for some reason, chisel3 cannot recognize the width here
              write_mask.apply(memNode.spmBankWidth - 1, 0).asBools()
            ))
            .map { case ((cow, wo), m) =>
              Mux(option_&&(Some(addr_cow_eq_wo), m), wo, cow)
            }
        // Assign to atomic update write
        option_:=(aupd_write_data, Some(VecInit(vec_back_forward_data).asUInt()))
      case _ =>
        read_data := Mux(addr_ro_eq_cor, aupd_write_data.get, read_port_data)
        option_:=(aupd_write_data, Some(Mux(addr_cow_eq_wo, write_data_reg, aupd_comp_result.get)))
    }
  }

  /* -------------------------     Finite State Machine     ------------------------- */

  /* -------------------------       Output Connection      ------------------------- */

  // Valid
  memResponse.valid := read_data_valid && RegNext(read_enable)
  // Address
  memResponse.addr := readAddrReg.getOrElse(0.U)
  option_:=(Some(memResponse.addr), readAddrReg)
  // Memory Operation
  memResponse.memOp := RegNext(memRequest.memOp)
  // Mask
  memResponse.mask := RegNext(memRequest.mask)
  // Data
  memResponse.data := read_data
  // Reorder Buffer ID
  memResponse.robId match {
    case Some(value) =>
      require(memRequest.robId.isDefined)
      value := RegNext(memRequest.robId.get)
    case None =>
  }
  // Request Vector Position
  memResponse.reqIdx match {
    case Some(value) =>
      require(memRequest.reqIdx.isDefined)
      value := RegNext(memRequest.reqIdx.get)
    case None =>
  }

  /* -------------------------     Hardware Sanity Check    ------------------------- */

  /* -------------------------            Utility           ------------------------- */

  /** Connect two optional hardware
    *
    * @param t    sink hardware
    * @param that source hardware
    * @tparam T type of hardware
    */
  def option_:=[T <: Data](
    t:          Option[T],
    that:       Option[T],
    widthCheck: Boolean = true,
    existCheck: Boolean = false
  ): Unit = {
    import dsagen2.util.StreamUtil.optConnect
    optConnect(t, that, widthCheck, existCheck)
  }

  /** Optional enable assignment, should be equal to option_RegEnable
    *
    * @param dest   optional destination
    * @param enable optional enable
    * @param orig   optional source
    * @tparam T type of signal
    */
  def option_enable_:=[T <: Data](dest: Option[T], enable: Option[Bool], orig: Option[T]): Unit = {
    (dest, enable, orig) match {
      case (Some(d), Some(b), Some(s)) =>
        require(d.getWidth == s.getWidth)
        when(b) {
          d := s
        }
      case _ =>
    }
  }

  def option_&&[T <: Bool](o1: Option[T], o2: T): Bool = {
    o1 match {
      case Some(o) => o && o2
      case None    => o2
    }
  }

  /* ------------------------- Post Generation Sanity Check ------------------------- */
}
