package dsagen2.util

import chisel3._
import chisel3.experimental.{BaseModule, DataMirror, IntParam}
import chisel3.internal.requireIsChiselType
import chisel3.util._
import dsagen2.util.Annotator.vivadoDontTouch

trait WithQueueIO[T <: Data] extends Module {
  val io: QueueIO[T]
}

class QueueFPGAIO(val w: Int) extends Bundle {
  // Clock and Reset
  val clk:   Bool = Input(Bool())
  val reset: Bool = Input(Bool())
  // Enqueue
  val if_full_n:   Bool = Output(Bool())
  val if_write_ce: Bool = Input(Bool())
  val if_write:    Bool = Input(Bool())
  val if_din:      UInt = Input(UInt(w.W))
  // Dequeue
  val if_empty_n: Bool = Output(Bool())
  val if_read_ce: Bool = Input(Bool())
  val if_read:    Bool = Input(Bool())
  val if_dout:    UInt = Output(UInt(w.W))
}

trait HasQueueFPGAIO extends BaseModule {
  val w: Int
  val io: QueueFPGAIO = IO(new QueueFPGAIO(w))
}

class QueueRelayStation(val w: Int, val entris: Int, val level: Int = 3)
    extends BlackBox(
      Map(
        "DATA_WIDTH" -> IntParam(w),
        "ADDR_WIDTH" -> IntParam(log2Ceil(entris)),
        "DEPTH" -> IntParam(entris),
        "LEVEL" -> IntParam(level)
      )
    )
    with HasQueueFPGAIO
    with HasBlackBoxResource {
  override def desiredName: String = "relay_station"
  addResource(blackBoxResource = s"/fpga/FIFO/relay_station.v")
}

class QueueBRAM(val w: Int, val entris: Int, val str: String)
    extends BlackBox(
      Map(
        "MEM_STYLE" -> str,
        "DATA_WIDTH" -> IntParam(w),
        "ADDR_WIDTH" -> IntParam(log2Ceil(entris)),
        "DEPTH" -> IntParam(entris)
      )
    )
    with HasQueueFPGAIO
    with HasBlackBoxResource {
  override def desiredName: String = "fifo_bram"

  addResource(blackBoxResource = s"/fpga/FIFO/fifo_bram.v")
}

class QueueFWD(val w: Int, val entris: Int, val str: String)
    extends BlackBox(
      Map(
        "MEM_STYLE" -> str,
        "DATA_WIDTH" -> IntParam(w),
        "ADDR_WIDTH" -> IntParam(log2Ceil(entris)),
        "DEPTH" -> IntParam(entris)
      )
    )
    with HasQueueFPGAIO
    with HasBlackBoxResource {
  override def desiredName: String = "fifo_fwd"

  addResource(blackBoxResource = s"/fpga/FIFO/fifo_fwd.v")
}

class QueueSRL(val w: Int, val entris: Int, val str: String)
    extends BlackBox(
      Map(
        "MEM_STYLE" -> str,
        "DATA_WIDTH" -> IntParam(w),
        "ADDR_WIDTH" -> IntParam(log2Ceil(entris)),
        "DEPTH" -> IntParam(entris)
      )
    )
    with HasQueueFPGAIO
    with HasBlackBoxResource {
  override def desiredName: String = "fifo_srl"

  addResource(blackBoxResource = s"/fpga/FIFO/fifo_srl.v")
}

class QueueFPGAInt[T <: Data](val gen: T, val entries: Int, val isFPGA: Boolean = false, val forceType: String = "")
    extends WithQueueIO[T] {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")

  /* ------------------------- Derived Parameters           ------------------------- */

  // Get the hardware type
  val genType: T = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }
  val w: Int = genType.getWidth

  /* ------------------------ Input / Output ------------------------ */
  val io = IO(new QueueIO(genType, entries))

  /* ------------------------ Implementation ------------------------ */
  val queue = if (isFPGA) {
    // Decide the Type
    val queueType: String = forceType match {
      case "" =>
        if (entries == 1) {
          // Forward
          "fwd"
        } else if (w >= 36 && entries >= 4096) {
          // URAM
          "uram"
        } else if (entries >= 128) {
          // BRAM
          "bram"
        } else {
          // SRL
          "srl"
        }
      case s: String => s
      case _ => require(requirement = false, s"Queue Type $forceType is not defined"); ""
    }

    // Create FPGA based Queue and return IO
    val fpgaQ = {
      queueType match {
        case "bram"  => Module(new QueueBRAM(w = w, entris = entries, str = "block"))
        case "uram"  => Module(new QueueBRAM(w = w, entris = entries, str = "ultra"))
        case "srl"   => Module(new QueueSRL(w = w, entris = entries, str = "shiftreg"))
        case "fwd"   => Module(new QueueFWD(w = w, entris = entries, str = ""))
        case "relay" => Module(new QueueRelayStation(w = w, entris = entries))
        case s: Any =>
          require(requirement = false, s"$s type of queue not defined")
          Module(new QueueFWD(w = w, entris = entries, str = ""))
      }
    }

    // Create counter for count
    val ctr: UInt = RegInit(0.U(log2Ceil(entries + 1).W))
    when((io.enq.fire() && io.deq.fire()) || (!io.enq.fire() && !io.deq.fire())) {
      // if enqueue or dequeue at the same time or NOT enqueue at the same time
      // DO nothing to the counter
      ctr := ctr
    }.elsewhen(io.enq.fire()) {
      ctr := ctr + 1.U
    }.elsewhen(io.deq.fire()) {
      ctr := ctr - 1.U
    }.otherwise {
      ctr := ctr
    }
    // Connect Clock, Reset and Count
    fpgaQ.io.clk := clock.asBool()
    fpgaQ.io.reset := reset.asBool()
    io.count := ctr
    // Enqueue
    io.enq.ready := fpgaQ.io.if_full_n
    fpgaQ.io.if_write_ce := true.B
    fpgaQ.io.if_write := io.enq.valid
    fpgaQ.io.if_din := io.enq.bits.asUInt()
    // Dequeue
    io.deq.valid := fpgaQ.io.if_empty_n
    io.deq.bits := fpgaQ.io.if_dout.asTypeOf(genType)
    fpgaQ.io.if_read := io.deq.ready
    fpgaQ.io.if_read_ce := true.B
    suggestName(s"QueueFPGA_${queueType}_w${w}_d$entries")
  } else {
    // Create queue
    val nativeQ = Module(new chisel3.util.Queue(genType, entries))
    // Enqueue
    nativeQ.io.enq.valid := io.enq.valid
    nativeQ.io.enq.bits := io.enq.bits
    io.enq.ready := nativeQ.io.enq.ready
    // Dequeue
    io.deq.bits := nativeQ.io.deq.bits
    io.deq.valid := nativeQ.io.deq.valid
    nativeQ.io.deq.ready := io.deq.ready
    // Count
    io.count := nativeQ.io.count
    suggestName(s"QueueNative_w${w}_d$entries")
    nativeQ
  }
}

class QueueFPGA[T <: Data](
  val gen:       T,
  val entries:   Int,
  val isFPGA:    Boolean = false,
  val dontTouch: Boolean = false,
  val qType:     String = "")
    extends WithQueueIO[T] {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")

  /* ------------------------- Derived Parameters           ------------------------- */

  // Get the hardware type
  val genType: T = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }
  val w: Int = genType.getWidth

  /* ------------------------ Input / Output ------------------------ */
  val io = IO(new QueueIO(genType, entries))

  /* ------------------------ Implementation ------------------------ */
  val queue = Module(new QueueFPGAInt[T](gen, entries, isFPGA, qType))
  if (dontTouch) { vivadoDontTouch(queue) }

  queue.io.enq.bits := io.enq.bits
  queue.io.enq.valid := io.enq.valid
  io.enq.ready := queue.io.enq.ready
  io.count := queue.io.count
  io.deq.bits := queue.io.deq.bits
  io.deq.valid := queue.io.deq.valid
  queue.io.deq.ready := io.deq.ready
}
