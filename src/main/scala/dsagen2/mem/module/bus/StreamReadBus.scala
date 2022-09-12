package dsagen2.mem.module.bus

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.mem.bundle._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.mem.diplomacy.{Mem2IVPParameter, OVP2MemParameter}
import dsagen2.util.UIntUtil.groupBitsAs
import dsagen2.util.RegUtil.RegNextN

/** Bus that sends stream response from ROB to input vector ports
  *
  * @param memNode   Memory node parameters
  * @param ivpsParam IVP parameters
  * @param ovpsParam OVP Parameters
  * @param p         CDE
  */
class StreamReadBus(
  val memNode:   MemNodeParameters,
  val ivpsParam: Seq[Mem2IVPParameter],
  val ovpsParam: Seq[OVP2MemParameter]
)(
  implicit val p: Parameters)
    extends MultiIOModule {

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  // I believe this module can only be used in DMA and SPM and GEN and REG,
  // rest nodes has different way to send data to read port
  // REC, just forward writeBundle
  // DIS, no read
  // Gen, mask is generated
  require(memNode.needStrResponse, s"Do you really need to do read like this?")

  /* ------------------------- Input / Output               ------------------------- */

  // Stream Response From ROB
  val strResponse: StreamResponse = IO(Input(new StreamResponse(memNode, ivpsParam, ovpsParam)))

  // Stream Accepted, reply the ivp readiness to ROB
  val strAccepted: Bool = IO(Output(Bool()))

  // All IVPs are read
  val allIVPReady: Bool = IO(Output(Bool()))

  // Input/Output, Read Ports
  // I, Input, ivpReady, readiness is checked at stream table level
  // I, Input, ivpReadyMask is actually useless
  // I, Input, number of available unit is currently not used, can be used later
  // Input, ivpBroadcast, whether vport is under broadcast setting
  // Input, ivpBroadcastIVPortId, the vport that each vport want to broadcast, broadcasting ready checked table
  val ivps: Seq[MemReadBundle] = ivpsParam.map { ivp => IO(new MemReadBundle(ivp)) }

  /* ------------------------- Wires                        ------------------------- */

  // Aggregated Stream Response where only low byte are active

  val num_level_merge : Int = log2Ceil(memNode.bandwidth)

  val aggStrRspData: Seq[Seq[Vec[UInt]]] =
    for(curr_level <- num_level_merge-1 to 0 by -1) yield {
      val num_node_curr_level : Int = math.pow(2, curr_level).toInt
      val num_word_per_node : Int = memNode.bandwidth/num_node_curr_level
      Seq.fill(num_node_curr_level)(Wire(Vec(num_word_per_node, UInt(memNode.memUnitBits.W))))
      //Wire(Vec(num_node_curr_level, UInt((memNode.bandBits/num_node_curr_level).W)))
    }

  val aggStrRspCount: Seq[Seq[UInt]] = {
    for(curr_level <- num_level_merge-1 to 0 by -1) yield {
      val num_node_curr_level : Int = math.pow(2, curr_level).toInt
      Seq.fill(num_node_curr_level)(Wire(UInt((num_level_merge-curr_level+1).W)))
      //Wire(Vec(num_node_curr_level, UInt((num_level_merge-curr_level+1).W)))
    }
  }
  require(aggStrRspCount.nonEmpty)
  aggStrRspCount.foreach (level => level.foreach (count => dontTouch(count)))

  // Get the ivp port that stream request points
  val targetPortIdx: UInt = strResponse.meta.targetLocalPortId.getOrElse(0.U)

  // Create Bit vector that indicate whether this stream response sends to input vector port
  val toIVPValidMask: Seq[Bool] =
    ivps.zipWithIndex.map { case (ivp, ivpIdx) =>
      // Point to this port or broadcast
      val toThisPort: Bool = targetPortIdx === ivpIdx.U
      val bcToThisPort: Bool = ivp.ivpBroadcastIVPortId match {
        case Some(bcIvpIdx) =>
          require(ivp.ivpBroadcast.isDefined)
          Mux(ivp.ivpBroadcast.get, targetPortIdx === bcIvpIdx, false.B)
        case None => false.B
      }
      // Connect the memory valid
      strResponse.valid && (toThisPort || bcToThisPort)
    }

  // Stream Response Fired
  val strFired: Bool =
    //VecInit(ivps.zip(toIVPValidMask).map { case (ivp, toThis) => Mux(toThis, ivp.ivpReady, true.B) }).asUInt().andR() &&
    VecInit(toIVPValidMask).asUInt().orR()

  /* ------------------------- Combination Logic            ------------------------- */

  // Merge Counts
  for(idx <- 0 until memNode.bandwidth/2) {
    require(aggStrRspCount.nonEmpty, s"Memory Node Bandwidth / 2 = ${memNode.bandwidth/2}")
    aggStrRspCount.head(idx) := strResponse.mask.get(idx*2) +& strResponse.mask.get(idx*2+1)
  }
  for(curr_level <- 1 until num_level_merge) {
    for(idx <- 0 until math.pow(2, num_level_merge-curr_level-1).toInt) {
      aggStrRspCount(curr_level)(idx) := RegNext(aggStrRspCount(curr_level-1)(2*idx) +& aggStrRspCount(curr_level-1)(2*idx+1))
    }
  }
  // Shift and Merge Bytes
  for(idx <- 0 until memNode.bandwidth/2) {
    when(!strResponse.mask.get(2*idx) && !strResponse.mask.get(2*idx+1)) {
      aggStrRspData.head(idx).head := 0.U
      aggStrRspData.head(idx)(1) := 0.U
    } .elsewhen(!strResponse.mask.get(2*idx) && strResponse.mask.get(2*idx+1)) {
      aggStrRspData.head(idx).head := strResponse.readDataGroup.get(2*idx+1)
      aggStrRspData.head(idx)(1) := 0.U
    } .elsewhen(strResponse.mask.get(2*idx) && !strResponse.mask.get(2*idx+1)) {
      aggStrRspData.head(idx).head := strResponse.readDataGroup.get(2*idx)
      aggStrRspData.head(idx)(1) := 0.U
    } .otherwise {
      aggStrRspData.head(idx).head := strResponse.readDataGroup.get(2*idx)
      aggStrRspData.head(idx)(1) := strResponse.readDataGroup.get(2*idx+1)
    }
  }
  for(curr_level <- 1 until num_level_merge) {
    for (idx <- 0 until math.pow(2, num_level_merge - curr_level - 1).toInt) {
      val leftNode: Vec[UInt] = Wire(Vec(math.pow(2, curr_level + 1).toInt, UInt(memNode.memUnitBits.W)))
      // Shift each word by count
      leftNode.zipWithIndex.foreach { case (word, jdx) =>
        // Generate shift mappings for MuxLookup
        val data: Seq[UInt] = aggStrRspData(curr_level - 1)(2 * idx + 1)
        val mapping: Seq[(UInt, UInt)] =
          for (shift_by <- 0 to data.length) yield {
            if (shift_by <= jdx && jdx - shift_by < data.length) {
              (shift_by.U, data(jdx - shift_by))
            } else {
              (shift_by.U, 0.U)
            }
          }
        word := MuxLookup(aggStrRspCount(curr_level - 1)(2 * idx), 0.U, mapping)
      }
      val rightNode: UInt = aggStrRspData(curr_level - 1)(2 * idx).asUInt()
      val result: UInt = Wire(UInt((2 * rightNode.getWidth).W))
      result := leftNode.asUInt() | rightNode
      val resultReg: UInt = Reg(UInt(result.getWidth.W))
      resultReg := result
      aggStrRspData(curr_level)(idx) := groupBitsAs(resultReg, memNode.memUnitBits)
    }
  }

  /* ------------------------- Output Connection            ------------------------- */

  // Aggregate memory response
  val aggStrRspReadData : Vec[UInt] = aggStrRspData(num_level_merge-1).head
  val aggStrRspMask : UInt = Wire(UInt(memNode.bandwidth.W))
  aggStrRspMask := (1.U << aggStrRspCount(num_level_merge-1).head).asUInt() - 1.U

  // by default stream is not accepted, unless that stream response's target
  // input vector port is ready to receive stream
  strAccepted := strFired

  // Connect to all IVPs are ready
  allIVPReady := VecInit(ivps.map(_.ivpReady)).asUInt().andR()

  ivps.zip(toIVPValidMask).foreach { case (ivp, ivpValid) =>
    // Connect Valid
    ivp.memValid := RegNextN(ivpValid && strFired, num_level_merge-1).last
    // Connect mask
    require(ivp.memValidMask.getWidth == aggStrRspMask.getWidth)
    ivp.memValidMask := aggStrRspMask
    // Connect Data
    require(ivp.memData.length == aggStrRspMask.getWidth)
    require(ivp.memData.length == aggStrRspReadData.length)
    ivp.memData.zip(aggStrRspReadData).zip(aggStrRspMask.asBools()).foreach {
      case ((ivpData, srData), valid) =>
        require(ivpData.getWidth == srData.getWidth, s"tData.value = ${ivpData.getWidth}, data = ${srData.getWidth}")
        ivpData := srData
    }
    // Used by memory is connected at stream table
    ivp.usedByMem := DontCare
    // Stream State
    ivp.memStreamState match {
      case Some(value) => value := RegNextN(strResponse.state.getOrElse(0.U.asTypeOf(new StreamState)), num_level_merge-1).last
      case None        =>
    }
    // Stream Linear Padding
    ivp.memPadding match {
      case Some(padding) => padding := RegNextN(strResponse.meta.linearPadding.get, num_level_merge-1).last
      case None          =>
    }
    // Broadcast reset is on when stream state reach the end and do broadcast to this port
    ivp.broadcastReset match {
      case Some(bcReset) =>
        // Reset the broadcast register
        bcReset := RegNextN(ivpValid && strFired &&
          strResponse.state.getOrElse(0.U.asTypeOf(new StreamState)).EndStr, num_level_merge-1).last
      case None =>
    }
  }
}
