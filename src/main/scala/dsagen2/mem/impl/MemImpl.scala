package dsagen2.mem.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.PopCount
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.mem.bundle.{MemReadBundle, MemReadPacket, MemWriteBundle, StreamState}
import dsagen2.mem.diplomacy.Mem2IVPParameter
import dsagen2.mem.module.bus.StreamReadBus
import dsagen2.mem.module.stab.BaseStrTable
import dsagen2.top.config.DSAFixedConfig.noPad
import dsagen2.top.diplomacy.DSANodeType.RecurrenceEngine
import dsagen2.util.StreamUtil.validReduceMin
import dsagen2.util.{QueueFPGA, WithQueueIO}

/** Common Function for memory node implementations
  */
object MemImpl {
  // Connect Output Vector Port Write Port to Stream Table
  def ovps2tab(tab: BaseStrTable, ovps: Seq[MemWriteBundle]): Unit = {
    require(ovps.length == tab.memWritePorts.length)
    ovps.zip(tab.memWritePorts).foreach { case (writePort, stabWritePort) =>
      writePort <> stabWritePort
    }
  }

  // Connect Read Ports to Stream Table for Readiness and used by memory checking
  // In isRecurrence mode, since if the IVP is not ready for accepting new read stream, Stream Table will retry
  // so the readiness checking is turn off here
  def ivps2tab(tab: BaseStrTable, ivps: Seq[MemReadBundle]): Unit = {
    require(ivps.length == tab.memReadPorts.length)
    val isRecurrence: Boolean = tab.memNode.nodeType == RecurrenceEngine
    ivps.zip(tab.memReadPorts).foreach { case (readPort, stabReadPort) =>
      // Here we should only connect the ivp port field that is used in Stream Table
      // ivpBroadcast, ivpBroadcastIVPortId, ivpReady, usedByMem
      readPort.usedByMem := stabReadPort.usedByMem
      // For recurrence engine, IVP readiness check will be ignore here
      stabReadPort.ivpReady := {
        if (!isRecurrence) readPort.ivpReady else true.B
      }
      // For recurrence engine, broadcast port readiness check will also be ignored
      stabReadPort.ivpBroadcast match {
        case Some(value) =>
          value := {
            if (!isRecurrence) readPort.ivpBroadcast.getOrElse(false.B) else false.B
          }
        case None =>
      }
      stabReadPort.ivpBroadcastIVPortId match {
        case Some(value) => value := readPort.ivpBroadcastIVPortId.getOrElse(0.U)
        case None        =>
      }
      // TODO: the number of available entry in ivp is not used for stream issue for now
      readPort <> stabReadPort
    }
  }

  // Connect stream read bus to each input vector port
  def bus2ivps(
    bus:       StreamReadBus,
    ivps:      Seq[MemReadBundle],
    ivpParams: Seq[Mem2IVPParameter]
  )(
    implicit p: Parameters
  ): Seq[(Bool, UInt, WithQueueIO[MemReadPacket])] = {
    require(bus.ivps.length == ivps.length)
    require(ivps.length == ivpParams.length)
    bus.ivps.zip(ivps).zip(ivpParams).map { case ((bus, ivp), param) =>
      // Relay Station
      val ivpQ =
        Module(new QueueFPGA[MemReadPacket](new MemReadPacket(param), 4, p(FPGAOverlay), dontTouch = false))

      // Bus <-> Relay Station
      bus.ivpReady := ivpQ.io.enq.ready
      ivpQ.io.enq.valid := bus.memValid
      ivpQ.io.enq.bits := bus
      bus.ivpReadyMask := DontCare // ready mask is useless
      bus.ivpAvailUnits := DontCare // Can be used later, but useless here
      bus.ivpBroadcast match {
        case Some(value) => value := ivp.ivpBroadcast.getOrElse(false.B)
        case None        =>
      }
      bus.ivpBroadcastIVPortId match {
        case Some(value) => value := ivp.ivpBroadcastIVPortId.getOrElse(0.U)
        case None        =>
      }
      bus.ivpCapa := DontCare
      bus.ivpLeftByte := DontCare

      // Relay Station <-> IVP
      ivpQ.io.deq.ready := ivp.ivpReady
      ivp.memValid := ivpQ.io.deq.valid
      ivp.memValidMask := ivpQ.io.deq.bits.memValidMask
      ivp.memData := ivpQ.io.deq.bits.memData
      ivp.memStreamState match {
        case Some(value) => value := ivpQ.io.deq.bits.memStreamState.getOrElse(0.U.asTypeOf(new StreamState))
        case None        =>
      }
      ivp.memPadding match {
        case Some(value) => value := ivpQ.io.deq.bits.memPadding.getOrElse(noPad)
        case None        =>
      }
      ivp.broadcastReset match {
        case Some(value) => value := ivpQ.io.deq.bits.broadcastReset.get
        case None        =>
      }

      // Return dequeue fire and dequeue number of byte
      (ivpQ.io.deq.fire(), PopCount(ivpQ.io.deq.bits.memValidMask), ivpQ)
    }
  }

  // Correct IVP Left Byte by taking broadcasting into consideration
  def broadcastLeftByte(ivpPorts: Seq[MemReadBundle]): Seq[UInt] = {
    // Taking out broadcast, broadcast IVP, left byte of each ivp
    val ivpStats: Seq[(Bool, UInt, UInt)] = ivpPorts.map { ivp =>
      (ivp.ivpBroadcast.getOrElse(false.B), ivp.ivpBroadcastIVPortId.getOrElse(0.U), ivp.ivpLeftByte)
    }
    // Loop over all IVP to find the minimum left byte among all ivp that broadcast from it
    val ivpsActLeft: Seq[UInt] = for (ivpIdx <- ivpPorts.indices) yield {
      // Get the left of itsself
      val selfLeft: UInt = ivpPorts(ivpIdx).ivpLeftByte
      // Get the statistic of other IVPs
      val otherStats: Seq[(Bool, UInt, UInt)] = ivpStats.patch(ivpIdx, Nil, 1)
      // Calculate whether it is doing broadcasting from this IVP
      val doBroadLeft: Seq[(Bool, UInt)] = otherStats.map { case (b, bivpIdx, left) =>
        (b && bivpIdx === ivpIdx.U, left)
      }
      val (beBroadcasted, minOtherLeft) = validReduceMin(doBroadLeft)
      Mux(beBroadcasted, selfLeft.min(minOtherLeft), selfLeft)
    }
    ivpsActLeft
  }
}
