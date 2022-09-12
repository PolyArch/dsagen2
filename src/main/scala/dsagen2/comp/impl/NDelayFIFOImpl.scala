package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.config.processing_element.PEDsaOperationParameters
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.comp.module.DelayPipe
import dsagen2.util.{QueueFPGA, WithQueueIO}

trait NDelayFIFOImpl[T <: Data] {
  val p:        Parameters
  val aluParam: PEDsaOperationParameters

  /* ----- Virtual Floating Wire ----- */
  val delayFifosIn:    Seq[DecoupledIO[T]]
  val delayFifosCount: Seq[UInt] // count is used as input in static, but output as dynamic to report current #element
  val delayFifosOut:   Seq[DecoupledIO[T]]
  val delayFifosBusy:  Seq[Bool]

  def delay(): Unit = {
    // Extract parameter from CDE
    val maxDelay:     Int = aluParam.maxFifoDepth
    val staticDelay:  Boolean = !aluParam.isDynamic
    val numDelayFIFO: Int = aluParam.maxNumOperand

    /* ---------- Static Delay FIFO ----------  */
    if (staticDelay) {
      val fifos: Seq[DelayPipe[T]] =
        Seq.fill(numDelayFIFO)(Module(new DelayPipe[T](chiselTypeOf(delayFifosIn.head.bits), maxDelay)))
      // Connect Input to FIFO
      delayFifosIn.zip(fifos).foreach { case (in, pipe) =>
        pipe.input.valid := in.valid
        require(pipe.input.bits.getWidth == in.bits.getWidth, s"Mismatch assignment")
        pipe.input.bits := in.bits
      }
      // Connect Delay to Pipe
      delayFifosCount.zip(fifos).foreach { case (delay, pipe) =>
        pipe.delay := delay
      }
      // Connect FIFO to output
      delayFifosOut.zip(fifos).foreach { case (out, pipe) =>
        out.valid := pipe.output.valid
        require(out.bits.getWidth == pipe.output.bits.getWidth, s"Mismatch assignment")
        out.bits := pipe.output.bits
      }
      delayFifosBusy.zip(fifos).foreach { case (busy, fifo) =>
        busy := fifo.busy
      }
      // Static FIFO is always ready
      delayFifosIn.foreach { in => in.ready := true.B }
      delayFifosOut.foreach { out => out.ready := DontCare }
    } else {
      /* ---------- Dynamic Queue FIFO ----------*/
      val fifos: Seq[WithQueueIO[T]] =
        Seq.fill(numDelayFIFO)(Module(new QueueFPGA[T](chiselTypeOf(delayFifosIn.head.bits), maxDelay, p(FPGAOverlay))))
      // Connect Input to FIFO
      fifos.zip(delayFifosIn).foreach { case (fifo, in) => fifo.io.enq <> in }
      // Connect Output to FIFO
      fifos.zip(delayFifosOut).foreach { case (fifo, out) => fifo.io.deq <> out }
      // Report the count to outside
      fifos.zip(delayFifosCount).foreach { case (queue, count) =>
        count := queue.io.count
      }
      // Busy is valid
      delayFifosBusy.zip(fifos).foreach { case (busy, fifo) =>
        busy := fifo.io.deq.valid
      }
    }
  }
}
