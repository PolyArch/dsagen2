package dsagen2.top.module

import chisel3._
import chisel3.util._
import dsagen2.comp.bundle.CompNodeStatus
import dsagen2.ctrl.bundle.CtrlNodeStatus
import dsagen2.mem.bundle.MemoryNodeStatus
import dsagen2.sync.bundle.VectorPortStatus
import dsagen2.top.config.DSAFixedConfig._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.XLen

class DSAMonitor(val dsa: DSAGen)(implicit p: Parameters) extends MultiIOModule {

  // Collect the number of cycle that single bit condition is true
  def getCycleSingleBit(
    width:   Int,
    cond:    Bool,
    isStart: Bool = isStart,
    isEnd:   Bool = isEnd,
    isClear: Bool = isClear
  ): UInt = {
    // Counter register
    val cycleReg: UInt = RegInit(0.U(width.W))
    // FSM
    when(isStart && cond) {
      cycleReg := cycleReg + 1.U
    }.elsewhen(isClear) {
      cycleReg := 0.U
    }
    // Return the
    cycleReg
  }

  // Collect memory node statistic based on memory type
  def getMemStatistic(memType: UInt): (UInt, UInt, UInt, UInt, UInt, UInt) = {
    (
      getCycleSingleBit(XLEN, VecInit(memStatuses.map(s => s.memType === memType && s.alive)).asUInt().orR()),
      getCycleSingleBit(XLEN, VecInit(memStatuses.map(s => s.memType === memType && s.newStr)).asUInt().orR()),
      getCycleSingleBit(XLEN, VecInit(memStatuses.map(s => s.memType === memType && s.aguReq)).asUInt().orR()),
      getCycleSingleBit(XLEN, VecInit(memStatuses.map(s => s.memType === memType && s.readPause)).asUInt().orR()),
      getCycleSingleBit(XLEN, VecInit(memStatuses.map(s => s.memType === memType && s.writePause)).asUInt().orR()),
      getCycleSingleBit(XLEN, VecInit(memStatuses.map(s => s.memType === memType && s.portPause)).asUInt().orR())
    )
  }

  /* ----------- Extract parameters ---------- */

  // CPU Width
  val XLEN: Int = p(XLen)

  // Quarter split the number of PE and Switch
  val quarterNumPe: Int = dsa.numPE / 4
  val quarterNumSw: Int = dsa.numSW / 4

  /* ----------- Inward ---------- */

  // Statistic trigger
  val recordStat: Valid[UInt] = IO(Flipped(ValidIO(UInt(XLEN.W))))

  // Compute Node Status
  val compStatuses: Vec[CompNodeStatus] = IO(Input(Vec(dsa.numCOMP, new CompNodeStatus)))

  // Sync Node Status, 0~numIVP: IVP Status, numIVP~numVP: OVP Status
  val syncStatuses: Vec[VectorPortStatus] = IO(Input(Vec(dsa.numVP, new VectorPortStatus)))

  // Memory Node Status
  val memStatuses: Vec[MemoryNodeStatus] = IO(Input(Vec(dsa.numMEM, new MemoryNodeStatus)))

  // Stream Dispatcher Status
  val ctrlStatus: CtrlNodeStatus = IO(Input(new CtrlNodeStatus))

  /* ---------- Outward ---------- */

  // Output Statistic
  val statistic: Valid[UInt] = IO(ValidIO(UInt(XLEN.W)))

  /* ---------- State Register ---------- */
  val startROI: Bool = recordStat.valid && recordStat.bits === 0.U
  val endROI:   Bool = recordStat.valid && recordStat.bits === 1.U
  val resetROI: Bool = recordStat.valid && recordStat.bits === 2.U
  val clear :: start :: end :: Nil = Enum(3)
  val state:   UInt = RegInit(0.U(2.W))
  val isStart: Bool = state === start
  val isEnd:   Bool = state === end
  val isClear: Bool = state === clear

  // Kick start the Finite State Machine
  when((isClear || isEnd) && startROI) {
    state := start
  }.elsewhen(isStart && endROI) {
    state := end
  }.elsewhen((isStart || isEnd) && resetROI) {
    state := clear
  }

  /* ---------- Wires ---------- */

  // Number of Processing Element that are busy
  val numPeBusy:  UInt = PopCount(compStatuses.map(x => x.busy && x.hwType === peCompType))
  val numPeFired: UInt = PopCount(compStatuses.map(x => x.fired && x.hwType === peCompType))

  // Number of Switch that are fired
  val numSwBusy:  UInt = PopCount(compStatuses.map(x => x.busy && x.hwType === swCompType))
  val numSwFired: UInt = PopCount(compStatuses.map(x => x.fired && x.hwType === swCompType))

  // Control Node Output
  val cycLoadBits:      UInt = getCycleSingleBit(XLEN, ctrlStatus.reloadBits)
  val cycConfNodes:     UInt = getCycleSingleBit(XLEN, ctrlStatus.configuringBits)
  val cycLoadLastBits:  UInt = getCycleSingleBit(XLEN, ctrlStatus.loadLastBits)
  val cycEmptyCmdQueue: UInt = getCycleSingleBit(XLEN, ctrlStatus.noValidStrEntry && ctrlStatus.noValidSyncEntry)
  val cycValidCmdQueue: UInt = getCycleSingleBit(XLEN, ctrlStatus.hasValidStrEntry || ctrlStatus.hasValidSyncEntry)
  val cycFullCmdQueue:  UInt = getCycleSingleBit(XLEN, ctrlStatus.fullValidStrEntry || ctrlStatus.fullValidSyncEntry)

  // Processing Element Statistic
  val cyc1stQuarterPeBusy: UInt = getCycleSingleBit(XLEN, numPeBusy < quarterNumPe.U)
  val cyc2ndQuarterPeBusy: UInt =
    getCycleSingleBit(XLEN, numPeBusy >= quarterNumPe.U && numPeBusy < (quarterNumPe * 2).U)
  val cyc3rdQuarterPeBusy: UInt =
    getCycleSingleBit(XLEN, numPeBusy >= (2 * quarterNumPe).U && numPeBusy < (3 * quarterNumPe).U)
  val cyc4thQuarterPeBusy:  UInt = getCycleSingleBit(XLEN, numPeBusy >= (3 * quarterNumPe).U)
  val cyc1stQuarterPeFired: UInt = getCycleSingleBit(XLEN, numPeFired < quarterNumPe.U)
  val cyc2ndQuarterPeFired: UInt =
    getCycleSingleBit(XLEN, numPeFired >= quarterNumPe.U && numPeFired < (quarterNumPe * 2).U)
  val cyc3rdQuarterPeFired: UInt =
    getCycleSingleBit(XLEN, numPeFired >= (2 * quarterNumPe).U && numPeFired < (3 * quarterNumPe).U)
  val cyc4thQuarterPeFired: UInt = getCycleSingleBit(XLEN, numPeFired >= (3 * quarterNumPe).U)

  // Switch Statistic
  val cyc1stQuarterSwBusy: UInt = getCycleSingleBit(XLEN, numSwBusy < quarterNumSw.U)
  val cyc2ndQuarterSwBusy: UInt =
    getCycleSingleBit(XLEN, numSwBusy >= quarterNumSw.U && numSwBusy < (quarterNumSw * 2).U)
  val cyc3rdQuarterSwBusy: UInt =
    getCycleSingleBit(XLEN, numSwBusy >= (2 * quarterNumSw).U && numSwBusy < (3 * quarterNumSw).U)
  val cyc4thQuarterSwBusy:  UInt = getCycleSingleBit(XLEN, numSwBusy >= (3 * quarterNumSw).U)
  val cyc1stQuarterSwFired: UInt = getCycleSingleBit(XLEN, numSwFired < quarterNumSw.U)
  val cyc2ndQuarterSwFired: UInt =
    getCycleSingleBit(XLEN, numSwFired >= quarterNumSw.U && numSwFired < (quarterNumSw * 2).U)
  val cyc3rdQuarterSwFired: UInt =
    getCycleSingleBit(XLEN, numSwFired >= (2 * quarterNumSw).U && numSwFired < (3 * quarterNumSw).U)
  val cyc4thQuarterSwFired: UInt = getCycleSingleBit(XLEN, numSwFired >= (3 * quarterNumSw).U)

  // Memory Node Statistic
  val (cycDmaAlive, cycDmaNewStr, cycDmaAguReq, cycDmaReadPause, cycDmaWritePause, cycDmaPortPause): (
    UInt,
    UInt,
    UInt,
    UInt,
    UInt,
    UInt
  ) = getMemStatistic(dmaMemType)
  val (cycSpmAlive, cycSpmNewStr, cycSpmAguReq, cycSpmReadPause, cycSpmWritePause, cycSpmPortPause): (
    UInt,
    UInt,
    UInt,
    UInt,
    UInt,
    UInt
  ) = getMemStatistic(spmMemType)
  val (cycRecAlive, cycRecNewStr, cycRecAguReq, cycRecReadPause, cycRecWritePause, cycRecPortPause): (
    UInt,
    UInt,
    UInt,
    UInt,
    UInt,
    UInt
  ) = getMemStatistic(recMemType)
  val (cycRegAlive, cycRegNewStr, cycRegAguReq, cycRegReadPause, cycRegWritePause, cycRegPortPause): (
    UInt,
    UInt,
    UInt,
    UInt,
    UInt,
    UInt
  ) = getMemStatistic(regMemType)
  val (cycGenAlive, cycGenNewStr, cycGenAguReq, cycGenReadPause, cycGenWritePause, cycGenPortPause): (
    UInt,
    UInt,
    UInt,
    UInt,
    UInt,
    UInt
  ) = getMemStatistic(genMemType)

  /* ---------- Combinational Logics ---------- */

  // The list of statistic
  val listStat: Seq[UInt] = Seq(
    // Control Node Overview Statistic
    cycLoadBits,
    cycConfNodes,
    cycLoadLastBits,
    cycEmptyCmdQueue,
    cycValidCmdQueue,
    cycFullCmdQueue,
    // Processing Element Statistic
    cyc1stQuarterPeBusy,
    cyc2ndQuarterPeBusy,
    cyc3rdQuarterPeBusy,
    cyc4thQuarterPeBusy,
    cyc1stQuarterPeFired,
    cyc2ndQuarterPeFired,
    cyc3rdQuarterPeFired,
    cyc4thQuarterPeFired,
    // Switch Statistic
    cyc1stQuarterSwBusy,
    cyc2ndQuarterSwBusy,
    cyc3rdQuarterSwBusy,
    cyc4thQuarterSwBusy,
    cyc1stQuarterSwFired,
    cyc2ndQuarterSwFired,
    cyc3rdQuarterSwFired,
    cyc4thQuarterSwFired,
    // DMA Node Statistic
    cycDmaAlive,
    cycDmaNewStr,
    cycDmaAguReq,
    cycDmaReadPause,
    cycDmaWritePause,
    cycDmaPortPause,
    // Scratchpad Node Statistic
    cycSpmAlive,
    cycSpmNewStr,
    cycSpmAguReq,
    cycSpmReadPause,
    cycSpmWritePause,
    cycSpmPortPause,
    // Recurrence Node Statistic
    cycRecAlive,
    cycRecNewStr,
    cycRecAguReq,
    cycRecReadPause,
    cycRecWritePause,
    cycRecPortPause,
    // Generate Node Statistic
    cycGenAlive,
    cycGenNewStr,
    cycGenAguReq,
    cycGenReadPause,
    cycGenWritePause,
    cycGenPortPause,
    // Register Node Statistic
    cycRegAlive,
    cycRegNewStr,
    cycRegAguReq,
    cycRegReadPause,
    cycRegWritePause,
    cycRegPortPause
  )

  // Construct lookup table, start from 3 since 0-2 are reserved for
  val statLUT: Seq[(UInt, UInt)] = listStat.zipWithIndex.map { case (stat, idx) =>
    (idx + 3).U -> stat
  }

  /* ---------- Output Connection ---------- */
  statistic.valid := RegNext(recordStat.valid)
  statistic.bits := RegNext(MuxLookup(recordStat.bits, 0.U, statLUT))
}
