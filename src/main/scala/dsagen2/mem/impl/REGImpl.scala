package dsagen2.mem.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.ctrl.bundle.StreamDispatchBus
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, MemReadNode, MemWriteNode, OVP2MemParameter}
import dsagen2.mem.impl.MemImpl._
import dsagen2.mem.module.stab.StreamTable
import dsagen2.top.diplomacy.DSANodeType.RegisterEngine
import dsagen2.util.StreamUtil.disp2stream
import freechips.rocketchip.tile.RoCCResponse

/** Implementation of Register Engine
  *
  * Function Description:
  * 1. Collect ONE value from Output Vector Ports, send it to CPU by using
  * 2. Communication between Register Engine is done via RoCC Response
  *
  * Connection Flow:
  * - Output Vector Ports  -> Stream Table : Collect data to be sent to CPU
  * - Stream Table         -> Register Update Response : Value to be sent back to CPU
  *
  * Attention:
  * ! Register Engine should not have input vector ports connects to it, it is the sink of stream, which does not produce
  * stream. For CPU->IVPs, it should be done by generate engine.
  */
object REGImpl {

  /** Circuit building for Register Engine
    *
    * @param strDisp      Stream Dispatch Port
    * @param memWriteNode Diplomatic Node, receive inward connection from OVPs
    * @param memReadNode  Diplomatic Node, produce outward connection to IVPs
    * @param rdResp       Register Destination Response Port, send value back to CPU
    * @param memStatus    Memory Status Report
    */
  def circuit(
    strDisp: StreamDispatchBus,
    // Inward, memory write node
    memWriteNode: MemWriteNode,
    // Outward, memory read node
    memReadNode: MemReadNode,
    // Register Update Response
    rdResp: DecoupledIO[RoCCResponse], // Outward
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

    /* ------------------------- Derived Parameters           ------------------------- */

    // Count the number of vector port
    def numIVP: Int = ivpsParam.length

    def numOVP: Int = ovpsParam.length

    /* ------------------------- Parameters Sanity Check      ------------------------- */

    // Node Type
    require(memNode.nodeType == RegisterEngine, s"Node Type ${memNode.nodeType} != Register Engine")

    // Do sanity check for memory node
    require(memNode.sanity, s"Memory node ${memNode.getNodeName} does not pass sanity check")

    // Number of VP connection check
    require(
      numOVP > 0 && numIVP == 0,
      s"Register Engine is only responsible for send OVP port data back to CPU, so at least one OVP is required, " +
        s"and #IVP should be zeor, but #OVP = ${numOVP}, #IVP = $numIVP"
    )

    /* ------------------------- Input / Output               ------------------------- */

    // Memory Write Ports
    val (memWritePorts, _): (Seq[MemWriteBundle], Seq[OVP2MemParameter]) = memWriteNode.in.unzip

    /* ------------------------- Modules                      ------------------------- */

    // Stream Table, Keep track of the readiness of data from OVPs, issue for respond to CPU
    // The Length 1D of Table should always be 1.U for valid stream entry
    // TODO: A optimization pass should be added to remove fields that are useless
    val tab: StreamTable = Module(new StreamTable(memNode, ivpsParam, ovpsParam))

    /* ------------------------- Wires                        ------------------------- */

    // Pick out the selected stream entry from stream table, this is all we need for Register Engine
    val selEntry: StreamEntry = tab.selStrEntry.currStrEntry.origStrEntry
    val selData:  UInt = tab.selStrEntry.writeData.asUInt()

    // Collect required fields to construct new stream entry from stream dispatcher
    val newStrEntry: StreamEntry = WireInit(0.U.asTypeOf(new StreamEntry(memNode, ivpsParam, ovpsParam)))
    disp2stream(newStrEntry, strDisp)

    /* ------------------------- Combination Logic            ------------------------- */

    // Connect newly dispatched stream entry to stream table
    tab.newStrEntry := newStrEntry

    // Connect Write Ports to Stream Table for Data Retrieve that to be sent to CPU
    ovps2tab(tab, memWritePorts)

    // Write pause is connected from RoCC response ready, since write back to CPU is also one kind of write
    tab.writePause.get := !rdResp.ready

    // Since all stream entry will only be selected once (data received from OVP and sent to CPU is scalar)
    // The selected stream entry will be dead in next cycle, all zeros means turn off
    tab.updStrEntry := 0.U.asTypeOf(new StreamEntryOutstanding(memNode, ivpsParam, ovpsParam))

    /* ------------------------- Output Connection            ------------------------- */

    // Connect selected stream entry to RD response port
    require(selEntry.rd.isDefined, s"Stream Entry of Register Engine should have rd defined")
    rdResp.valid := selEntry.valid
    rdResp.bits.rd := selEntry.rd.get
    rdResp.bits.data := selData

    // Report memory statue
    memStatus := tab.memStatus
  }
}
