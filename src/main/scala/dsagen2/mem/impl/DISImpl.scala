package dsagen2.mem.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle.{MemReadBundle, MemWriteBundle, MemoryNodeStatus, StreamEntry}
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, MemReadNode, MemWriteNode, OVP2MemParameter}
import dsagen2.mem.impl.MemImpl.ovps2tab
import dsagen2.mem.module.agu.StreamARGU
import dsagen2.mem.module.stab.StreamTable
import dsagen2.util.StreamUtil.disp2stream

/** Discard Engine Implementation
  */
object DISImpl {

  /** Circuit Building for Discard Engine
    *
    * Connection Flow:
    * Output Vector Ports  -> Stream Table (check how many element need to be discarded)
    * Stream Table        <-> AGU (update remaining length 1D)
    * AGU                  -> Nowhere (discarded)
    *
    * @param strDisp      Stream Dispatcher Port
    * @param memWriteNode Memory Write Node
    * @param memReadNode  Memory Read Node
    * @param memStatus    Report Memory Status back to stream dispatcher
    */
  def circuit(
    strDisp: StreamDispatchBus,
    // Inward, memory write node
    memWriteNode: MemWriteNode,
    // Outward, memory read node
    memReadNode: MemReadNode,
    // Outward, Report Memory Status
    memStatus: MemoryNodeStatus,
    // Memory Node Parameter
    memNode: MemNodeParameters
  )(
    implicit p: Parameters
  ): Unit = {
    /* ------------------------- Extract Parameters           ------------------------- */

    // Memory Read Parameters
    val (_, ivpsParam): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Parameters
    val (_, ovpsParam): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* ------------------------- Derived Parameters           ------------------------- */

    // Count IVP and OVP
    def numIVP: Int = ivpsParam.length

    def numOVP: Int = ovpsParam.length

    /* ------------------------- Parameters Sanity Check      ------------------------- */

    // Memory Node Sanity Check
    require(memNode.sanity, s"Memory node ${memNode.getNodeName} does not pass sanity check")

    // Memory Node Type
    require(memNode.isDIS, s"Implementation should only be applied to Discard Engine")

    // Make sure that discard engine has very simple stream feature
    require(
      !memNode.supportIndirect && memNode.numLinearDimension == 1 && memNode.MaxAbsStride1D == 1 &&
        !memNode.streamStated && !memNode.LinearPadding && memNode.AtomicOperations.isEmpty &&
        memNode.numGenDataType == 0 && memNode.numPendingRequest == 0 && memNode.numSpmBank == 0,
      s"$memNode is not legal Discard Engine parameters"
    )

    // At least one OVP, no IVP
    require(
      numOVP > 0 && numIVP == 0,
      s"Discard engine must accept at least one OVP and cannot connect to IVP, " +
        s"but #OVP = $numOVP, #IVP = $numIVP"
    )

    /* ------------------------- Input / Output               ------------------------- */

    // Memory Read Ports (should be none, discard engine is the sink of stream, it will never produce stream
    val (_, _): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Ports, should be at least one
    val (memWritePorts, _): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* ------------------------- Modules                      ------------------------- */

    // Stream Table (keep track the remaining number of element to be discarded)
    val tab: StreamTable = Module(new StreamTable(memNode, ivpsParam, ovpsParam))

    // Address Generation (just used for calculating the remaining length 1D)
    val agu: StreamARGU = Module(new StreamARGU(memNode, ivpsParam, ovpsParam))

    /* ------------------------- Wires                        ------------------------- */

    // Stream Entry from Stream Dispatcher
    val newStrEntry: StreamEntry = WireInit(0.U.asTypeOf(new StreamEntry(memNode, ivpsParam, ovpsParam)))
    disp2stream(newStrEntry, strDisp)

    /* ------------------------- Combination Logic            ------------------------- */

    // Dispatch from Stream Dispatcher to Stream Table
    tab.newStrEntry := newStrEntry

    // Connect OVPs to stream table to gather output data stream for discard
    ovps2tab(tab, memWritePorts)

    // Stream Table selects stream entry to AGU, for calculation of remaining length 1D
    agu.forceKill := false.B // Discard engine has no data dependent feature, so stream association is always disabled
    agu.selStrEntry := tab.selStrEntry

    // Stream Table receive stream whose Length 1D is updated
    tab.updStrEntry := agu.updStrEntry

    // Stream Request from AGU is ignored here, so called "discarded"

    /* ------------------------- Output Connection            ------------------------- */

    // Report memory engine status
    memStatus := tab.memStatus
  }
}
