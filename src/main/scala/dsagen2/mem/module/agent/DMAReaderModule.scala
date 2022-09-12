package dsagen2.mem.module.agent

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.{MemRequest, MemResponse}
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.module.tlb.FrontendTLBIOClient
import dsagen2.top.config.DSAFixedConfig.memOpRead
import dsagen2.top.diplomacy.DSANodeType.DirectMemoryAccess
import dsagen2.util.StreamUtil.clearLowBits
import dsagen2.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.tilelink._

/** Direct Memory Access Reader Agent, Read can always be OoO since there is no RAR conflict. Response
  * order is maintained by ROB from upstream
  *
  * @param memNode Memory Node Parameters
  * @param p       CDE
  */
class DMAReaderModule(memNode: MemNodeParameters)(implicit p: Parameters) extends LazyModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  // Number of Pending DMA Request
  val robSize: Int = memNode.numPendingRequest

  /* ------------------------- Derived Parameters           ------------------------- */

  // Calculate full beatByte lgSize
  val fullLgSize: Int = log2Ceil(memNode.bandwidth)

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // Memory Node Type should be DMA
  require(memNode.nodeType == DirectMemoryAccess, s"Only DMA need this module, but it is ${memNode.nodeType}")

  /* ---------- Diplomacy Node ----------*/
  val node = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        Seq(TLMasterParameters.v1(s"dsagen-dma-reader-${memNode.getNodeId}", IdRange(0, robSize)))
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

    // O, DMA Read beatData Response sent to ROB
    val memResponse: MemResponse = IO(Output(new MemResponse(memNode)))

    // O, TLB Port that provides Virtual Address to Physical Address Translation
    val tlbPort: FrontendTLBIOClient = IO(new FrontendTLBIOClient)

    /* ------------------------- Registers                    ------------------------- */

    /* ------------------------- Modules                      ------------------------- */

    // Low priority arbiter to select between retry request and new request
    val tlb_arb: Arbiter[TLBundleAWithInfo] = Module(new Arbiter(new TLBundleAWithInfo, 2))

    // Queue to buffer untranslated to request that will be sent to TLB request port
    val tlb_q: Queue[TLBundleAWithInfo] = Module(new Queue(new TLBundleAWithInfo, entries = 1, pipe = true))

    // Queue to buffer request that has already sent translation request to TLB
    // Request in translation may still not be translated
    // entry = 1 becuase tlb taks 1 cycle to give response
    val translate_q: Queue[TLBundleAWithInfo] = Module(new Queue(new TLBundleAWithInfo, entries = 1, pipe = true))

    /* ------------------------- Wires                        ------------------------- */

    // Generate the TileLink Get Bundle
    val get: TLBundleA = edge
      .Get(
        fromSource = memRequest.robId.get,
        toAddress = 0.U, // this should be translated
        lgSize = fullLgSize.U // Request a full beatByte
      )
      ._2

    // Request TLBundle Type
    class TLBundleAWithInfo extends Bundle {
      val tl_a:   TLBundleA = DataMirror.internal.chiselTypeClone[TLBundleA](tl.a.bits)
      val vaddr:  UInt = Output(UInt(vaddrBits.W))
      val status: MStatus = Output(new MStatus)
    }

    // Wire that holds the new request in TLBundleA form
    val untranslated_a: DecoupledIO[TLBundleAWithInfo] = Wire(Decoupled(new TLBundleAWithInfo))

    // Queue that holds the untranslated request
    // Since this queue has the same depth with ROB, so it will never be full
    // Back pressure is handle by ROB
    val untrans_queue: WithQueueIO[TLBundleAWithInfo] = Module(
      new QueueFPGA(new TLBundleAWithInfo, entries = memNode.numPendingRequest, p(FPGAOverlay))
    )

    // 0 goes to retries, 1 goes to state machine
    val retry_a: DecoupledIO[TLBundleAWithInfo] = Wire(Decoupled(new TLBundleAWithInfo))

    /* ------------------------- Combination Logic            ------------------------- */

    // Convert new request to TLBundle
    untranslated_a.valid := memRequest.valid && memRequest.isRead
    untranslated_a.bits.tl_a := get
    untranslated_a.bits.vaddr := memRequest.vaddr
    untranslated_a.bits.status := memRequest.mStatus.get
    untrans_queue.io.enq <> untranslated_a

    // Select between retry request and new request, retry has higher priority
    tlb_arb.io.in(0) <> retry_a
    tlb_arb.io.in(1) <> untrans_queue.io.deq
    tlb_q.io.enq <> tlb_arb.io.out

    // Connect TLB Request Queue to Translation Queue
    translate_q.io.enq <> tlb_q.io.deq

    // We always dequeue from translated
    translate_q.io.deq.ready := true.B

    // Retry the tlb miss request, translated request may still results in TLB miss
    retry_a.valid := translate_q.io.deq.valid && (tlbPort.resp.miss || !tl.a.ready)
    retry_a.bits := translate_q.io.deq.bits

    /* ------------------------- Finite State Machine         ------------------------- */

    /* ------------------------- Output Connection            ------------------------- */

    // TLB Port Request
    tlbPort.req.valid := tlb_q.io.deq.valid
    tlbPort.req.bits.tlb_req.vaddr := tlb_q.io.deq.bits.vaddr
    tlbPort.req.bits.tlb_req.passthrough := false.B
    tlbPort.req.bits.tlb_req.size := 0.U // send_size
    tlbPort.req.bits.tlb_req.cmd := M_XRD
    tlbPort.req.bits.status := tlb_q.io.deq.bits.status

    // Send translated read request
    tl.a.valid := translate_q.io.deq.valid && !tlbPort.resp.miss
    tl.a.bits := translate_q.io.deq.bits.tl_a
    tl.a.bits.address := clearLowBits(tlbPort.resp.paddr, fullLgSize)

    // The back pressure is checked in upstream readPause from ROB to stream table, so it is always
    // ready
    tl.d.ready := true.B

    // Connect to memory response from TL.d
    memResponse.valid := tl.d.valid
    memResponse.addr := DontCare // Read Addr at this stage is useless
    memResponse.memOp := memOpRead
    memResponse.mask := Fill(memResponse.mask.getWidth, 1.U(1.W)) // Full Lg Size lead to this
    require(memResponse.data.getWidth == tl.d.bits.data.getWidth)
    require(tl.d.bits.data.getWidth == memNode.bandBits)
    memResponse.data := tl.d.bits.data
    require(memResponse.robId.isDefined)
    memResponse.robId.get := tl.d.bits.source
    require(memResponse.reqIdx.isEmpty, s"You have multiple req port to DMA?")

    /* ------------------------- Hardware Sanity Check        ------------------------- */

    /* ------------------------- Utility                      ------------------------- */

    /* ------------------------- Post Generation Sanity Check ------------------------- */

  }
}
