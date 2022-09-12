package dsagen2.mem.module.bank

import chisel3._
import chisel3.util._
import dsagen2.util.RegUtil.RegNextN
import dsagen2.util.UIntUtil.groupBitsAs

class DualPort1w1rSyncReadRAMBlackBox_IO(
  val unitBits:  Int = 8,
  val widthBits: Int,
  val depth:     Int,
  val maskWrite: Boolean)
    extends Bundle {
  /*---------- Write ----------*/
  // Write Clock
  val clka: Bool = Input(Bool())
  // Write Enable
  val ena: Bool = Input(Bool())
  // Write Mask
  val wea: UInt = if (maskWrite) Input(UInt((widthBits / unitBits).W)) else Input(UInt(1.W))
  // Write Address
  val addra: UInt = Input(UInt(log2Ceil(depth).W))
  // Data Input (Write)
  val dina: UInt = Input(UInt(widthBits.W))

  /*----------  Read ----------*/
  // Read Clock
  val clkb: Bool = Input(Bool())
  // Read Enable
  val enb: Bool = Input(Bool())
  // Read Address
  val addrb: UInt = Input(UInt(log2Ceil(depth).W))
  // Data Output (Read)
  val doutb: UInt = Output(UInt(widthBits.W))
}

class DualPort1w1rSyncReadRAMBlackBox(
  val unitBits:  Int = 8,
  val widthBits: Int,
  val depth:     Int,
  val maskWrite: Boolean,
  val latency:   Int)
    extends BlackBox
    with HasBlackBoxResource {
  val moduleName: String = s"BRAM_${widthBits}w_${depth}d_${latency}l_1w1r"

  override def desiredName: String = moduleName

  val io: DualPort1w1rSyncReadRAMBlackBox_IO = IO(
    new DualPort1w1rSyncReadRAMBlackBox_IO(unitBits, widthBits, depth, maskWrite)
  )
  addResource(blackBoxResource = s"/fpga/$moduleName/sim/$moduleName.v")
}

class DualPort1w1rSyncReadRAM(
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
  suggestName(s"DualPortSyncReadRAM_u${unitBits}_w${widthBits}_d$depth${wen}l$latency")
  // Derived Parameter
  val depthBits: Int = log2Ceil(depth)
  // IO
  /*---------- Write ----------*/
  val writeEnable: Bool = IO(Input(Bool()))
  val writeMask:   UInt = if (maskWrite) IO(Input(UInt((widthBits / unitBits).W))) else writeEnable.asUInt()
  val writeAddr:   UInt = IO(Input(UInt(depthBits.W)))
  val writeData:   UInt = IO(Input(UInt(widthBits.W)))
  /*----------  Read ----------*/
  val readEnable: Bool = IO(Input(Bool()))
  val readAddr:   UInt = IO(Input(UInt(depthBits.W)))
  val readData:   UInt = IO(Output(UInt(widthBits.W)))
  // Implementation
  if (isFPGA) {
    // FPGA IP Black Box Implementation
    val blackBox: DualPort1w1rSyncReadRAMBlackBox =
      Module(new DualPort1w1rSyncReadRAMBlackBox(unitBits, widthBits, depth, maskWrite, latency))
    // Clock
    blackBox.io.clka := clock.asBool()
    blackBox.io.clkb := clock.asBool()
    // Write
    blackBox.io.ena := writeEnable
    blackBox.io.wea := writeMask
    blackBox.io.addra := writeAddr
    blackBox.io.dina := writeData
    // Read
    blackBox.io.enb := readEnable
    blackBox.io.addrb := readAddr
    readData := blackBox.io.doutb
  } else {
    // ASIC / Behaviour Model
    val mem: SyncReadMem[Vec[UInt]] =
      SyncReadMem(
        depth, {
          if (maskWrite) Vec(widthBits / unitBits, UInt(unitBits.W)) else Vec(1, UInt(widthBits.W))
        },
        SyncReadMem.WriteFirst
      )
    // Write
    when(writeEnable) {
      mem.write(writeAddr, VecInit(groupBitsAs(writeData, unitBits)), writeMask.asBools())
    }
    // Read
    if (latency > 1) {
      readData := RegNextN(mem.read(readAddr, readEnable).asUInt(), latency - 1).last
    } else {
      readData := mem.read(readAddr, readEnable).asUInt()
    }
  }
}
