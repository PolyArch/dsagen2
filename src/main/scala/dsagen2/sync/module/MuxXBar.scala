package dsagen2.sync.module

import chisel3._
import chisel3.util._
import dsagen2.util.AppUtil.{bitvecSynchronized, gen2genType}

/** A MUX based crossbar without changing the data type
  *
  * Selection : 0 -> not selecting any input, 1 -> select first input port
  * Input Ready : if none of the output select this input port, it will be false. If output port that selects this input
  *  port exist, then wait for all output port to be ready
  *
  * @param gen The hardware type of input / output
  * @param numInput Number of souce
  * @param numOutput Number of sink
  * @param circuitEquality if true, means actually no crossbar, just pass through
  * @tparam T Data Type
  */
class MuxXBar[T <: Data](val gen: T, val numInput: Int, val numOutput: Int, val circuitEquality: Boolean = false)
    extends MultiIOModule {

  // Requirement
  require(numInput > 0 && numOutput > 0, s"Input and Output are problematic : #input = $numInput, #output = $numOutput")

  // Get the chisel type of hardware
  val genType: T = gen2genType(gen, compileOptions)

  // IO
  val vecInput:  Vec[DecoupledIO[T]] = IO(Flipped(Vec(numInput, DecoupledIO(genType))))
  val vecOutput: Vec[DecoupledIO[T]] = IO(Vec(numOutput, DecoupledIO(genType)))
  val sels: Seq[UInt] =
    Seq.fill(numOutput)(IO(Input(UInt(log2Ceil(numInput + 1).W)))) // +1 means that zero select ground

  if (circuitEquality) {
    require(numInput == numOutput, s"Non XBar implementation, but numInput $numInput != numOutput $numOutput")
    vecInput.zip(vecOutput).foreach { case (in, out) => in <> out }
  } else {
    // Generate Input OneHot per output side (bits and valid), 0 means select nowhere
    val out1Hs: Seq[Seq[Bool]] = sels.map { outSel => (1 to numInput).map(_.U === outSel) }

    // Generate Output OneHot per input side (ready), 0 means select nowhere
    val in1Hs: Seq[Seq[Bool]] = (1 to numInput).map { inSel => sels.map(x => x === inSel.U) }

    // Pick out the input wire
    val inBits:     Seq[T] = vecInput.map(_.bits)
    val inValids:   Seq[Bool] = vecInput.map(_.valid)
    val outReadies: Seq[Bool] = vecOutput.map(_.ready)

    // Input Ready
    require(vecInput.length == in1Hs.length)
    vecInput.zip(in1Hs).foreach { case (input, in1H) =>
      val selected:       Bool = VecInit(in1H).asUInt().orR()
      val selectAllReady: Bool = VecInit(bitvecSynchronized(outReadies, Some(in1H))).asUInt().orR()
      input.ready := selected & selectAllReady
    }

    // Output connection for bits and valid
    require(vecOutput.length == out1Hs.length)
    vecOutput.zip(out1Hs).foreach { case (output, out1H) =>
      val hasSource: Bool = VecInit(out1H).asUInt().orR()
      output.bits := Mux(hasSource, Mux1H(out1H, inBits), 0.U.asTypeOf(genType))
      output.valid := hasSource & Mux1H(out1H, inValids)
    }
  }
}
