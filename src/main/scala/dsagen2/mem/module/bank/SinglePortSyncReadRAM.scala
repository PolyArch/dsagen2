package dsagen2.mem.module.bank

import chisel3._
import chisel3.util._

class SinglePortSyncReadRAMBlackBox_IO(
  val unitBits:  Int = 8,
  val widthBits: Int,
  val depth:     Int,
  val maskWrite: Boolean)
    extends Bundle {
  // Clock
  val clka: Bool = Input(Bool())
  // Enable
  val ena: Bool = Input(Bool())
  // Write Enable
  val wea: UInt = if (maskWrite) Input(UInt((widthBits / unitBits).W)) else Input(UInt(1.W))
  // Address
  val addra: UInt = Input(UInt(log2Ceil(depth).W))
  // Data Input (Write)
  val dina: UInt = Input(UInt(widthBits.W))
  // Data Output (Read)
  val douta: UInt = Output(UInt(widthBits.W))
}

class SinglePortSyncReadRAMBlackBox(
  val unitBits:  Int = 8,
  val widthBits: Int,
  val depth:     Int,
  val maskWrite: Boolean,
  val latency:   Int)
    extends BlackBox
    with HasBlackBoxResource {
  val moduleName: String = s"BRAM_${widthBits}w_${depth}d_${latency}l"

  override def desiredName: String = moduleName

  val io: SinglePortSyncReadRAMBlackBox_IO = IO(
    new SinglePortSyncReadRAMBlackBox_IO(unitBits, widthBits, depth, maskWrite)
  )
  addResource(blackBoxResource = s"/fpga/$moduleName/sim/$moduleName.v")
}

/** Write First Synchronized Read RAM
  *
  * @param unitBits  Memory unit bits
  * @param widthBits Memory width bits
  * @param depth     Depth
  * @param maskWrite Mask write enable
  * @param latency   Latency to get read data
  * @param isFPGA    whether it is FPGA IP implementation
  */
class SinglePortSyncReadRAM(
  val unitBits:  Int = 8,
  val widthBits: Int,
  val depth:     Int,
  val maskWrite: Boolean = true,
  val latency:   Int,
  val isFPGA:    Boolean)
    extends MultiIOModule {
  // Sanity check
  require(widthBits % unitBits == 0, s"Width bits $widthBits must be multiple of unit bits $unitBits")
  require(isPow2(depth), s"Depth should be power of 2, but it is $depth")
  require(latency > 0, s"Sync Read Mem must have latency larger than 0")
  // Setup name
  val wen: String = if (maskWrite) "_wen_" else "_"
  suggestName(s"SinglePortSyncReadRAM_u${unitBits}_w${widthBits}_d$depth${wen}l$latency")

  // Parameter Derive
  def depthBits: Int = log2Ceil(depth)

  // IO
  val enable:      Bool = IO(Input(Bool()))
  val writeEnable: UInt = if (maskWrite) IO(Input(UInt((widthBits / unitBits).W))) else IO(Input(UInt(1.W)))
  val address:     UInt = IO(Input(UInt(depthBits.W)))
  val writeData:   UInt = IO(Input(UInt(widthBits.W)))
  val readData:    UInt = IO(Output(UInt(widthBits.W)))
  // Implementation
  if (isFPGA) { // FPGA IP BlackBox implementation
    val blackBox: SinglePortSyncReadRAMBlackBox =
      Module(new SinglePortSyncReadRAMBlackBox(unitBits, widthBits, depth, maskWrite, latency))
    blackBox.io.clka := clock.asBool()
    blackBox.io.ena := enable
    blackBox.io.wea := writeEnable
    blackBox.io.addra := address
    blackBox.io.dina := writeData
    readData := blackBox.io.douta
  } else { // ASIC Implementation (behavior model)
    // Memory Block
    val mem: SyncReadMem[Vec[UInt]] =
      SyncReadMem(
        depth, {
          if (maskWrite) Vec(widthBits / unitBits, UInt(unitBits.W)) else Vec(1, UInt(widthBits.W))
        },
        SyncReadMem.WriteFirst
      )
    // Implementation
    import dsagen2.util.RegUtil.RegNextN
    import dsagen2.util.UIntUtil.groupBitsAs
    val syncReadData: UInt = mem.read(address, !writeEnable.orR() && enable).asUInt()
    when(enable) {
      when(writeEnable.orR()) {
        // doing write first
        mem.write(address, VecInit(groupBitsAs(writeData, unitBits)), writeEnable.asBools())
      }
    }
    readData := RegNextN(syncReadData, latency - 1, List(syncReadData)).last
  }
}
