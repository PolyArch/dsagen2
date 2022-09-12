package dsagen2.mem.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle.{MemReadBundle, MemWriteBundle, MemoryNodeStatus, StreamEntry}
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, MemReadNode, MemWriteNode, OVP2MemParameter}
import dsagen2.mem.impl.MemImpl.{bus2ivps, ivps2tab, ovps2tab}
import dsagen2.mem.module.agu.StreamARGU
import dsagen2.mem.module.bus.StreamReadBus
import dsagen2.mem.module.ngu.StreamNGU
import dsagen2.mem.module.stab.StreamTable
import dsagen2.top.diplomacy.DSANodeType.GenerateEngine
import dsagen2.util.StreamUtil.disp2stream

/** Generate Engine Implementation
  */
object GENImpl {

  /** Building the circuit for GEN (generate engine)
    *
    * Connection Flow:
    * -- Output Vector Ports  -> Stream Table (for indirectly only)
    * -- Stream Table         -> Sequence Generator (Address Generator (AGU) + Numerical Generator Unit (NGU))
    * -- Key different here is that starting address will be added together with number generated from Mask
    * -- Sequence Generator   -> Input Vector Port Bus
    *
    * This object should contain module and connections between them
    *
    * @param strDisp      Stream Dispatch Port
    * @param memWriteNode Memory Write Node
    * @param memReadNode  Memory Read Node
    * @param memStatus    Memory Status Report Port
    */
  def circuit( // Inward, Me
    strDisp: StreamDispatchBus,
    // Inward, memory write node
    memWriteNode: MemWriteNode,
    // Outward, memory read node
    memReadNode: MemReadNode,
    // Outward, Report Memory Status
    memStatus: MemoryNodeStatus,
    // Parameters
    memNode: MemNodeParameters
  )(
    implicit p: Parameters
  ): Unit = {
    /* -------------------------      Extract Parameters      ------------------------- */

    // Memory Read Parameters
    val (_, ivpsParam): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Parameters
    val (_, ovpsParam): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* -------------------------    Parameters Sanity Check   ------------------------- */

    // Do sanity check for memory node
    require(memNode.sanity, s"Memory node ${memNode.getNodeName} does not pass sanity check")

    // Generate Engine is the source of stream, so there must be at least one memory read ports
    // but must not have memory write ports; It can have OVPs if indirect generation is supported
    require(
      (ivpsParam.nonEmpty && ovpsParam.isEmpty) ||
        (ivpsParam.nonEmpty && memNode.supportIndirect && ovpsParam.nonEmpty),
      s"Generate engine is the source of stream, it must " +
        s"connect to at least one IVP but ${ivpsParam.length}; " +
        s"it must not have OVP connects to it, but ${ovpsParam.length}"
    )

    // Double check the node type
    require(
      memNode.nodeType == GenerateEngine,
      s"This is Generation Engine implementation, but node type is ${memNode.nodeType}"
    )

    /* -------------------------        Input / Output        ------------------------- */

    // Memory Read Ports (should be at least one)
    val (memReadPorts, _): (Seq[MemReadBundle], Seq[Mem2IVPParameter]) = memReadNode.out.unzip

    // Memory Write Ports, should be empty
    val (memWritePorts, _): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* -------------------------     Modules Instantiation    ------------------------- */

    // Stream Table
    val tab: StreamTable = Module(new StreamTable(memNode, ivpsParam, ovpsParam))

    // Address Generation Unit
    val agu: StreamARGU = Module(new StreamARGU(memNode, ivpsParam, ovpsParam))

    // Numerical Sequence Generation Unit
    val ngu: StreamNGU = Module(new StreamNGU(memNode, ivpsParam, ovpsParam))

    // Stream Read Bus
    val bus: StreamReadBus = Module(new StreamReadBus(memNode, ivpsParam, ovpsParam))

    /* ------------------------- Wires                        ------------------------- */

    // Stream Entry from Stream Dispatcher
    val newStrEntry: StreamEntry = WireInit(0.U.asTypeOf(new StreamEntry(memNode, ivpsParam, ovpsParam)))
    disp2stream(newStrEntry, strDisp)

    /* -------------------------      Combination Logic       ------------------------- */

    // Dispatch from Stream Dispatcher to Stream Table
    tab.newStrEntry := newStrEntry

    // Connect Write Ports to Stream Table for Data Retrieve for Indirect Stream Generation
    if (memNode.isGEN && memNode.supportIndirect) {
      ovps2tab(tab, memWritePorts)
    }

    // Connect Read Ports to Stream Table for Readiness and used by memory checking
    ivps2tab(tab, memReadPorts)

    // Stream Table selects stream entry to AGU
    agu.forceKill := false.B // TODO: force kill by stream association
    agu.selStrEntry := tab.selStrEntry

    // AGU sends the updated stream entry back to stream table
    tab.updStrEntry := agu.updStrEntry

    // AGU sends the stream request directly to NGU
    ngu.strRequest := agu.strReqPorts.head

    // NGU sends the stream response to Bus
    bus.strResponse := ngu.strResponse
    ngu.strAccepted := bus.strAccepted

    // Connect stream read bus to each input vector port
    bus2ivps(bus, memReadPorts, ivpsParam)

    // Backpressure: stop read if NGU cannot take more request
    require(tab.readPause.isDefined)
    tab.readPause.get := ngu.convertPause

    /* -------------------------      Output Connection       ------------------------- */

    // Report the memory status
    memStatus := tab.memStatus
  }
}
