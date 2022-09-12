package dsagen2.mem.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, MemReadNode, MemWriteNode, OVP2MemParameter}
import dsagen2.mem.impl.MemImpl.{bus2ivps, ivps2tab, ovps2tab}
import dsagen2.mem.module.bus.StreamReadBus
import dsagen2.mem.module.stab.RecStreamTable
import dsagen2.util.QueueFPGA
import dsagen2.util.StreamUtil.disp2stream

/** Recurrence Engine Implementation
  */
object RECImpl {

  /** Circuit Building for Recurrence Engine
    *
    * Connection Flow:
    * Output Vector Ports  -> Stream Table (Data Collection)
    * Stream Table         -> Input vector ports (Recurrence, write data as read data in stream response)
    *
    * @param strDisp      Stream Dispatch Port
    * @param memWriteNode Memory Write Node
    * @param memReadNode  Memory Read Node
    * @param memStatus    Report Memory Status back to Stream Dispatcher
    */
  def circuit(
    strDisp: StreamDispatchBus,
    // Inward, memory write node
    memWriteNode: MemWriteNode,
    // Outward, memory read node
    memReadNode: MemReadNode,
    // Report Memory Status
    memStatus: MemoryNodeStatus, // Outward
    // Parameters
    memNode: MemNodeParameters
  )(
    implicit p: Parameters
  ): Unit = {
    /* ------------------------- Extract Parameters           ------------------------- */

    // Memory Read Parameters
    val (_, ivpsParam): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Parameters
    val (_, ovpsParam): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* ------------------------- Parameters Sanity Check      ------------------------- */

    // Memory Node Sanity Check
    require(memNode.sanity, s"Memory node ${memNode.getNodeName} does not pass sanity check")

    // Node Type
    require(memNode.isREC, s"Node ${memNode.getNodeName} should be recurrence engine")

    // Recurrence Engine should only be linear 1D
    require(
      memNode.numLinearDimension == 1 && !memNode.supportIndirect,
      s"Recurrence should just be linear 1D, " +
        s"but #linear = ${memNode.numLinearDimension}, #indirect = ${memNode.numIndirectDimension}"
    )

    /* ------------------------- Input / Output               ------------------------- */

    // Memory Read Ports (should be at least one)
    val (memReadPorts, _): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Ports, (should be at least one)
    val (memWritePorts, _): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* ------------------------- Modules                      ------------------------- */

    // Stream Table
    val tab: RecStreamTable = Module(new RecStreamTable(memNode, ivpsParam, ovpsParam))

    // Stream Read Bus to send stream response to input vector port
    val bus: StreamReadBus = Module(new StreamReadBus(memNode, ivpsParam, ovpsParam))

    /* ------------------------- Wires                        ------------------------- */

    // Stream Entry from Stream Dispatcher
    val newStrEntry: StreamEntry = WireInit(0.U.asTypeOf(new StreamEntry(memNode, ivpsParam, ovpsParam)))
    disp2stream(newStrEntry, strDisp)

    /* -------------------------      Combination Logic       ------------------------- */

    // Dispatch from Stream Dispatcher to Stream Table
    tab.newStrEntry := newStrEntry

    // Connect OVPs to stream table to gather write data for recurrence
    ovps2tab(tab, memWritePorts)

    // Connect Read Ports to Stream Table for Readiness checking and used by memory
    ivps2tab(tab, memReadPorts)

    // Pipe the stream response from stream table
    val rspQueue = Module(
      new QueueFPGA[StreamResponse](new StreamResponse(memNode, ivpsParam, ovpsParam), 2, p(FPGAOverlay), dontTouch = false)
    )

    // Connect Stream Request from AGU to request queue, stop (writePause) AGU when queue is full
    rspQueue.io.enq.valid := tab.strResponse.valid
    rspQueue.io.enq.bits := tab.strResponse
    tab.strAccepted := rspQueue.io.enq.ready

    // Convert stream response from response queue to bus
    bus.strResponse := rspQueue.io.deq.bits
    bus.strResponse.valid := rspQueue.io.deq.bits.valid && rspQueue.io.deq.valid
    rspQueue.io.deq.ready := bus.strAccepted || bus.allIVPReady

    // Connect stream read bus to each input vector ports
    bus2ivps(bus, memReadPorts, ivpsParam)

    /* ------------------------- Output Connection            ------------------------- */

    // Report the memory statue of recurrence engine
    memStatus := tab.memStatus
  }
}
