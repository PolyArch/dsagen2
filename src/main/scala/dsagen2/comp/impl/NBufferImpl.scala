package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.config.CompKeys.OutputBuffer
import dsagen2.comp.config.common.CompNodeOutputBufferParameters
import dsagen2.comp.impl.ip.FPGAOverlay
import dsagen2.util.{QueueFPGA, WithQueueIO}

trait NBufferImpl[BufferData <: Data] {
  implicit val p: Parameters
  val outputBufferParam: Option[CompNodeOutputBufferParameters] = p.lift(OutputBuffer)

  // In order to prevent short cut of circuit, if the output buffer parameter is not defined
  // A output register will be used
  def bufferDepth: Int = outputBufferParam match {
    case Some(value) => value.outputBufferDepth
    case None        => 1
  }

  def depthBits: Int = log2Ceil(bufferDepth + 1)

  /* ----- Virtual IO ----- */
  val buffersInput:  Seq[DecoupledIO[BufferData]]
  val buffersOutput: Seq[DecoupledIO[BufferData]]
  val buffersCount:  Seq[UInt]

  /** Buffer the input implementation
    *
    * @return Dequeue output ports bits
    */
  def buffers(): Unit = {
    // If output buffer parameter is not defined or output buffer is static output register, the output buffer depth
    // will not be used. A one cycle delay output register will be used
    if (outputBufferParam.isEmpty || outputBufferParam.get.staticOutputBuffer) {
      // Connect the wire
      buffersInput.zip(buffersOutput).zip(buffersCount).foreach { case ((input, output), count) =>
        output.valid := RegNext(input.valid)
        output.bits := RegNext(input.bits)
        count := RegNext(input.valid)
        input.ready := true.B
      }
    } else {
      // The number of buffer input and output should be same
      require(buffersOutput.length == buffersInput.length && buffersOutput.nonEmpty)
      val numBuffer: Int = buffersInput.length

      // Buffer Modules
      val bufferModules: Seq[WithQueueIO[BufferData]] =
        Seq.fill(numBuffer)(
          Module(new QueueFPGA[BufferData](chiselTypeOf(buffersInput.head.bits), bufferDepth, p(FPGAOverlay)))
        )

      // Connect the wire
      buffersInput.zip(buffersOutput).zip(bufferModules.zip(buffersCount)).foreach {
        case ((input, output), (buffer, count)) =>
          // Input
          buffer.io.enq <> input
          // Output
          output <> buffer.io.deq
          // Output the counter
          count := buffer.io.count
      }
    }
  }
}
