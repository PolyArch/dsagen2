package dsagen2.mem.module.agent

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.MemRequest
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.module.tlb.FrontendTLBIOClient
import dsagen2.util.StreamUtil._
import dsagen2.util.{QueueFPGA, WithQueueIO}
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.tilelink._

class DMAWriterModule(
  memNode: MemNodeParameters
)(
  implicit p: Parameters)
    extends LazyModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  // Number of Pending DMA Request, I know there is no ROB needed in writer,
  // but we can still use the same term to specify the number of pending write request
  val robSize: Int = memNode.numPendingRequest

  /* ------------------------- Derived Parameters           ------------------------- */

  // Calculate full beatByte lgSize
  val fullLgSize: Int = log2Ceil(memNode.bandwidth)

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  /* ---------- Diplomacy Node ----------*/
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        Seq(TLMasterParameters.v1(s"dsagen-dma-writer-${memNode.getNodeId}", IdRange(0, robSize)))
      )
    )
  )

  /* ---------- Module Implementation ---------*/
  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {

    /* ------------------------- Input / Output               ------------------------- */

    // Get the TileLink Bundle and its Edge Parameters
    val (tl, edge): (TLBundle, TLEdgeOut) = node.out.head

    // I, Input, DMA Read Request from ROB
    val memRequest: MemRequest = IO(Input(new MemRequest(memNode)))

    // O, TLB Port that provides Virtual Address to Physical Address Translation
    val tlbPort: FrontendTLBIOClient = IO(new FrontendTLBIOClient)

    // Pause write request
    val writePause: Bool = IO(Output(Bool()))

    /* ------------------------- Registers                    ------------------------- */

    // Register that keep track of each pending write
    val xactBusy: UInt = RegInit(0.U(robSize.W))

    /* ------------------------- Modules                      ------------------------- */

    /* ------------------------- Wires                        ------------------------- */

    // Whether there is new write request
    val newWrite: Bool = memRequest.valid && memRequest.isWrite

    // Wire class for TLBundleA request
    class TLBundleAWithInfo extends Bundle {
      val tl_a:   TLBundleA = DataMirror.internal.chiselTypeClone[TLBundleA](tl.a.bits)
      val vaddr:  UInt = Output(UInt(vaddrBits.W))
      val status: MStatus = Output(new MStatus)
    }

    // Wire, tracking the xact id
    val xactOnehot: UInt = PriorityEncoderOH(~xactBusy)
    val xactId:     UInt = OHToUInt(xactOnehot)
    val xactBusy_fire = WireInit(false.B)
    val xactBusy_add:    UInt = Mux(xactBusy_fire, (1.U << xactId).asUInt(), 0.U)
    val xactBusy_remove: UInt = (~Mux(tl.d.fire(), (1.U << tl.d.bits.source).asUInt(), 0.U)).asUInt()

    // Since we generated the mask before, so we always put partial
    val putPartial: TLBundleA = edge
      .Put(
        fromSource = RegEnableThru(xactId, memRequest.valid),
        toAddress = 0.U,
        lgSize = fullLgSize.U,
        data = memRequest.data,
        mask = memRequest.mask
      )
      ._2

    /* ------------------------- Combination Logic            ------------------------- */

    // Update the xact bust state
    xactBusy := (xactBusy | xactBusy_add) & xactBusy_remove

    // Request to TileLink
    val untranslated_a: DecoupledIO[TLBundleAWithInfo] = Wire(Decoupled(new TLBundleAWithInfo))
    xactBusy_fire := untranslated_a.fire() && newWrite
    untranslated_a.valid := newWrite && !xactBusy.andR()
    untranslated_a.bits.tl_a := putPartial
    untranslated_a.bits.vaddr := memRequest.vaddr
    untranslated_a.bits.status := memRequest.mStatus.get

    // 0 goes to retries, 1 goes to state machine
    val retry_a:        DecoupledIO[TLBundleAWithInfo] = Wire(Decoupled(new TLBundleAWithInfo))
    val shadow_retry_a: WithQueueIO[TLBundleAWithInfo] = Module(new QueueFPGA(new TLBundleAWithInfo, 1, p(FPGAOverlay)))
    shadow_retry_a.io.enq.valid := false.B
    shadow_retry_a.io.enq.bits := DontCare
    val tlb_arb: Arbiter[TLBundleAWithInfo] = Module(new Arbiter(new TLBundleAWithInfo, 3))
    tlb_arb.io.in(0) <> retry_a
    tlb_arb.io.in(1) <> shadow_retry_a.io.deq
    tlb_arb.io.in(2) <> untranslated_a

    val tlb_q: Queue[TLBundleAWithInfo] = Module(new Queue(new TLBundleAWithInfo, 1, pipe = true))
    tlb_q.io.enq <> tlb_arb.io.out

    tlbPort.req.valid := tlb_q.io.deq.fire()
    tlbPort.req.bits.tlb_req.vaddr := tlb_q.io.deq.bits.vaddr
    tlbPort.req.bits.tlb_req.passthrough := false.B
    tlbPort.req.bits.tlb_req.size := 0.U // send_size
    tlbPort.req.bits.tlb_req.cmd := M_XWR
    tlbPort.req.bits.status := tlb_q.io.deq.bits.status

    val translate_q: Queue[TLBundleAWithInfo] = Module(new Queue(new TLBundleAWithInfo, 1, pipe = true))
    translate_q.io.enq <> tlb_q.io.deq
    when(retry_a.valid) {
      translate_q.io.enq.valid := false.B
      shadow_retry_a.io.enq.valid := tlb_q.io.deq.valid
      shadow_retry_a.io.enq.bits := tlb_q.io.deq.bits
    }
    translate_q.io.deq.ready := tl.a.ready || tlbPort.resp.miss

    retry_a.valid := translate_q.io.deq.valid && tlbPort.resp.miss
    retry_a.bits := translate_q.io.deq.bits
    assert(!(retry_a.valid && !retry_a.ready))

    tl.a.valid := translate_q.io.deq.valid && !tlbPort.resp.miss
    tl.a.bits := translate_q.io.deq.bits.tl_a
    tl.a.bits.address := RegEnableThru(clearLowBits(tlbPort.resp.paddr, fullLgSize), RegNext(tlbPort.req.fire()))

    tl.d.ready := xactBusy.orR()

    /* ------------------------- Finite State Machine         ------------------------- */

    /* ------------------------- Output Connection            ------------------------- */

    // Pause the write request if there is only one left
    writePause := xactBusy.andR() || xactId >= (robSize - 1).U

    /* ------------------------- Hardware Sanity Check        ------------------------- */

    /* ------------------------- Utility                      ------------------------- */

    // This function will return "next" with a 0-cycle delay when the "enable" signal is high. It's like a queue with
    // the "pipe" and "flow" parameters set to "true"
    def RegEnableThru[T <: Data](next: T, enable: Bool): T = {
      val buf = RegEnable(next, enable)
      Mux(enable, next, buf)
    }

    /* ------------------------- Post Generation Sanity Check ------------------------- */
  }
}
