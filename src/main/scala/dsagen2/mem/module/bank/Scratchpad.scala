package dsagen2.mem.module.bank

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.{MemRequest, MemResponse}
import dsagen2.mem.config.{MemNodeParameters, SpadAluParams}
import dsagen2.top.dsa.MemBuild
import dsagen2.util.RegUtil.RegNextN

class Scratchpad(memNode: MemNodeParameters, dsagen: MemBuild)(implicit val p: Parameters) extends MultiIOModule {
  /* -------------------------      Extract Parameters      ------------------------- */

  /* -------------------------     Derived Parameters       ------------------------- */

  // Whether or not support Atomic Operation
  def supportAtomOp: Boolean = memNode.supportAtomicOperation

  // Number of Atomic Operation Data Type
  def numAtomDataTypeExp: Int = if (supportAtomOp) memNode.numMemDataTypeExp else 0

  // The scratchpad read latency
  def readLatency: Int = 2

  // Scratchpad Bank Width
  def bankWidth: Int = memNode.spmBankWidth

  // Memory unit bit width
  def memUnitBits: Int = memNode.memUnitBits

  // Depth of each scratchpad bank
  def bankDepth: Int = ((memNode.capacity / memNode.numSpmBank) / bankWidth).toInt

  /* -------------------------      Parameters Sanity Check ------------------------- */

  /* -------------------------         Input / Output       ------------------------- */

  // Request Port
  val memRequest: MemRequest = IO(Input(new MemRequest(memNode)))
  val reqReady:   Bool = IO(Output(Bool()))

  // Response Port
  val memResponse: MemResponse = IO(Output(new MemResponse(memNode)))

  /* -------------------------     Registers and Modules    ------------------------- */

  // Single Port SyncReadMemory Bank
  val mem: DualPort1w1rSyncReadRAM =
    Module(
      new DualPort1w1rSyncReadRAM(memUnitBits, bankWidth * memUnitBits, bankDepth, true, readLatency, p(FPGAOverlay))
    )

  // Pipe of memory request
  val requestPipe: List[MemRequest] = RegNextN(memRequest, readLatency, List(memRequest))

  // Optional ALU for atomic update enabled scratchpad
  val alu: Option[SPMBankALU] =
    if (supportAtomOp) Some(Module(new SPMBankALU(SpadAluParams(memNode), dsagen))) else None

  /* ------------------------- Wires / Combination Logic    ------------------------- */

  // Compute Input Side
  if (supportAtomOp) alu.get.comp_enable := requestPipe.last.isAtomOp
  if (supportAtomOp) alu.get.exp match {
    case Some(e) => e := requestPipe.last.memDataTypeExp.getOrElse(0.U);
    case _       =>
  }
  if (supportAtomOp) alu.get.opcode match {
    case Some(e) => e := requestPipe.last.memOp;
    case _       =>
  }
  if (supportAtomOp) alu.get.operand0 := mem.readData
  if (supportAtomOp) alu.get.operand1 := requestPipe.last.data

  // Compute Output Side
  val compWriteEnable: Bool = if (supportAtomOp) alu.get.result.valid else false.B
  val compWriteMask:   UInt = if (supportAtomOp) RegNext(requestPipe.last.mask) else 0.U
  val compWriteAddr:   UInt = if (supportAtomOp) RegNext(requestPipe.last.vaddr) else 0.U(memRequest.vaddr.getWidth.W)
  val compWriteData:   UInt = if (supportAtomOp) alu.get.result.bits else 0.U

  // Write connection
  mem.writeEnable := memRequest.isWrite || RegNext(requestPipe.last.isAtomOp)
  mem.writeMask := Mux(compWriteEnable, compWriteMask, memRequest.mask)
  mem.writeAddr := Mux(compWriteEnable, addr2Depth(compWriteAddr), addr2Depth(memRequest.vaddr))
  mem.writeData := Mux(compWriteEnable, compWriteData, memRequest.data)

  // Read Connection
  mem.readEnable := memRequest.isRead || memRequest.isAtomOp
  mem.readAddr := addr2Depth(memRequest.vaddr)

  /* ------------------------- Output Connection            ------------------------- */

  // Stop request if there is atomic update in pipe
  reqReady := {
    if (supportAtomOp) !(VecInit(requestPipe.map(x => x.isAtomOp)).asUInt().orR() || compWriteEnable) else true.B
  }

  // Memory response, only read request has it
  memResponse.valid := requestPipe.last.isRead
  memResponse.addr := requestPipe.last.vaddr
  memResponse.mask := requestPipe.last.mask
  memResponse.memOp := requestPipe.last.memOp
  memResponse.data := mem.readData
  memResponse.robId match {
    case Some(value) => value := requestPipe.last.robId.getOrElse(0.U);
    case _           =>
  }
  memResponse.reqIdx match {
    case Some(value) => value := requestPipe.last.reqIdx.getOrElse(0.U)
    case _           =>
  }

  def addr2Depth(addr: UInt): UInt = addr.apply(addr.getWidth - 1, memNode.lowerPosRowAddr)
}
