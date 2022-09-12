package dsagen2.mem.module.tlb

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.{MStatus, TLBPTWIO, TLBReq, TLBResp}
import freechips.rocketchip.tile.{CoreBundle, CoreModule}
import freechips.rocketchip.tilelink.TLEdgeOut

class DecoupledTLBReq(val lgMaxSize: Int)(implicit p: Parameters) extends CoreBundle {
  val tlb_req = new TLBReq(lgMaxSize)
  val status = new MStatus
}

class TLBExceptionIO extends Bundle {
  val interrupt:   Bool = Output(Bool())
  val flush_retry: Bool = Input(Bool())
  val flush_skip:  Bool = Input(Bool())

  def flush(): Bool = flush_retry || flush_skip
}

class FrontendTLBIOClient(implicit p: Parameters) extends CoreBundle {
  val lgMaxSize: Int = log2Ceil(coreDataBytes)
  val req:       ValidIO[DecoupledTLBReq] = Valid(new DecoupledTLBReq(lgMaxSize))
  val resp:      TLBResp = Flipped(new TLBResp)
}

class FrontendTLBIO(val nClients: Int)(implicit p: Parameters) extends CoreBundle {
  val clients: Vec[FrontendTLBIOClient] = Flipped(Vec(nClients, new FrontendTLBIOClient))
  val ptw = new TLBPTWIO
  val exp = new TLBExceptionIO
}

class FrontendTLB(nClients: Int, entries: Int, maxSize: Int)(implicit edge: TLEdgeOut, p: Parameters)
    extends CoreModule {
  val io:        FrontendTLBIO = IO(new FrontendTLBIO(nClients))
  val lgMaxSize: Int = log2Ceil(coreDataBytes)
  val tlbArb:    RRArbiter[DecoupledTLBReq] = Module(new RRArbiter(new DecoupledTLBReq(lgMaxSize), nClients))
  val tlb:       DecoupledTLB = Module(new DecoupledTLB(entries, maxSize))
  tlb.io.req.valid := tlbArb.io.out.valid
  tlb.io.req.bits := tlbArb.io.out.bits
  tlbArb.io.out.ready := true.B

  io.ptw <> tlb.io.ptw
  io.exp <> tlb.io.exp

  io.clients.zip(tlbArb.io.in).foreach { case (client, req) =>
    val last_translated_valid = RegInit(false.B)
    val last_translated_vpn = RegInit(0.U(vaddrBits.W))
    val last_translated_ppn = RegInit(0.U(paddrBits.W))

    val l0_tlb_hit = last_translated_valid &&
      ((client.req.bits.tlb_req.vaddr >> pgIdxBits).asUInt() ===
        (last_translated_vpn >> pgIdxBits).asUInt())
    val l0_tlb_paddr = Cat(last_translated_ppn >> pgIdxBits, client.req.bits.tlb_req.vaddr(pgIdxBits - 1, 0))

    when(req.fire() && !tlb.io.resp.miss) {
      last_translated_valid := true.B
      last_translated_vpn := req.bits.tlb_req.vaddr
      last_translated_ppn := tlb.io.resp.paddr
    }
    when(io.exp.flush()) {
      last_translated_valid := false.B
    }

    req.valid := RegNext(client.req.valid && !l0_tlb_hit)
    req.bits := RegNext(client.req.bits)

    when(!req.fire()) {
      client.resp := DontCare
      client.resp.paddr := RegNext(l0_tlb_paddr)
      client.resp.miss := !RegNext(l0_tlb_hit)
    }.otherwise {
      client.resp := tlb.io.resp
    }
  }
}
