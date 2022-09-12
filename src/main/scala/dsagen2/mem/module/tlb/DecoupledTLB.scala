package dsagen2.mem.module.tlb

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.CoreModule
import freechips.rocketchip.tilelink.TLEdgeOut

class DecoupledTLBIO(val lgMaxSize: Int)(implicit p: Parameters) extends Bundle {
  val req: ValidIO[DecoupledTLBReq] = Flipped(Valid(new DecoupledTLBReq(lgMaxSize)))
  val resp = new TLBResp
  val ptw = new TLBPTWIO
  val exp = new TLBExceptionIO
}

class DecoupledTLB(entries: Int, maxSize: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {

  val lgMaxSize: Int = log2Ceil(maxSize)
  val io = new DecoupledTLBIO(lgMaxSize)

  val interrupt: Bool = RegInit(false.B)
  io.exp.interrupt := interrupt

  val tlb: TLB = Module(new TLB(false, lgMaxSize, TLBConfig(nSets = 1, nWays = entries)))
  tlb.io.req.valid := io.req.valid
  tlb.io.req.bits := io.req.bits.tlb_req
  io.resp := tlb.io.resp
  tlb.io.kill := false.B

  tlb.io.sfence.valid := io.exp.flush()
  tlb.io.sfence.bits.rs1 := false.B
  tlb.io.sfence.bits.rs2 := false.B
  tlb.io.sfence.bits.addr := DontCare
  tlb.io.sfence.bits.asid := DontCare

  io.ptw <> tlb.io.ptw
  tlb.io.ptw.status := io.req.bits.status
  val exception: Bool = io.req.valid && Mux(
    io.req.bits.tlb_req.cmd === M_XRD,
    tlb.io.resp.pf.ld || tlb.io.resp.ae.ld,
    tlb.io.resp.pf.st || tlb.io.resp.ae.st
  )
  when(exception) {
    interrupt := true.B
  }
  when(interrupt && tlb.io.sfence.fire()) {
    interrupt := false.B
  }

  assert(!io.exp.flush_retry || !io.exp.flush_skip, "TLB: flushing with both retry and skip at same time")
}
