package dsagen2.mem.module.stab

import chisel3._
import chisel3.util._
import dsagen2.util.StreamUtil.validReduceMin

/** Stream Scheduler that selects the stream whose #entry in the ROB is minimum
  *
  * @param strSize  Size of the stream table
  * @param robSize  Size of the ROB
  * @param readSize Size of read stream, only read stream can use ROB
  */
class ROBStrScheduler(strSize: Int, robSize: Int, readSize: Int, agu2robPipeDep: Int = 2) extends MultiIOModule {
  /* ------------------------- Parameters Sanity Check      ------------------------- */
  require(strSize > 0, s"Stream Table size should be positive but it is $strSize")
  require(robSize > 0, s"ROB size should be positive, but it is $robSize")
  require(strSize >= readSize, s"Stream Table size $strSize should be larger than read stream size $readSize")

  /* ------------------------- Input / Output               ------------------------- */

  // Stream Entry Valid from Stream Table
  val strValids: UInt = IO(Input(UInt(strSize.W)))

  // ROB Entry Valid from ROB
  val robValids: UInt = IO(Input(UInt(robSize.W)))

  // ROB Port ID (Stream ID)
  val robStrIDs: Vec[UInt] = IO(Input(Vec(robSize, UInt(log2Ceil(readSize).W))))

  // Stream ready for issue because its Port ID has minimum entry in ROB
  val strRobIssueValids: UInt = IO(Output(UInt(strSize.W)))

  // ROB Stream Active (Count larger than 1), this port is to report stream aliveness when all stream dequeued from table
  // majorly used for holding status for VPs
  val readStrActs: UInt = IO(Output(UInt(readSize.W)))

  /* ------------------------- Combination Logic            ------------------------- */

  // Get the shared part of the ROB
  val robShareEmpties: Seq[Bool] = robValids.asBools().slice(readSize, robSize).map(!_)

  // Calculate number of left entry in shared part of ROB
  val robShareLeft: UInt = PopCount(robShareEmpties)

  // Count the valid entry in ROB for each stream
  val readStrCounts: Seq[UInt] = (0 until readSize).map { portId =>
    PopCount(robValids.asBools().zip(robStrIDs).map { case (valid, id) => valid && id === portId.U })
  }

  // Get the minimum, valid count
  val readValids: Seq[Bool] = strValids.asBools().slice(0, readSize)
  require(readValids.length == readSize)
  require(readStrCounts.length == readSize)
  val (minExist, minCount): (Bool, UInt) = validReduceMin(readValids.zip(readStrCounts))

  // Get read issue valid based on minimum ROB entry
  val enoughRoom: Bool = robShareLeft >= (agu2robPipeDep + 2).U
  val readIssueValids: Seq[Bool] = readValids.zip(readStrCounts).zipWithIndex.map { case ((valid, count), readIdx) =>
    // A conservative issue strategy, assuming all stream in pipe is for same port
    // TODO: parameterize 4 here, which is the pipeline distance from stream entry valid to input of ROB
    // stream select: 1 stage; output to selected: 1 stage; AGU to ROB: 2
    // Requirement check
    require(readIdx < robValids.getWidth, s"Read Index = $readIdx less than ROB size ${robValids.getWidth}")
    // Counter that make sure every dedicated slot availability can only be passed every 4 cycle
    val cnt: Counter = Counter(4)
    when(!enoughRoom && !robValids(readIdx) && readValids(readIdx)) {
      cnt.inc()
    }.otherwise {
      cnt.reset()
    }
    // Read stream can be issued
    // When ROB is plenty empty, then try to see if it is most thirsty one,
    // otherwise check dedicated slot
    Mux(
      enoughRoom, /*!minExist || valid && count === minCount*/ true.B,
      (cnt.value === 0.U) && !robValids(readIdx) && readValids(readIdx)
    )
  }

  // Connect to output
  strRobIssueValids := VecInit(readIssueValids ++ Seq.fill(strSize - readSize)(true.B)).asUInt()
  readStrActs := VecInit(readStrCounts.map(_ > 0.U)).asUInt()
}
