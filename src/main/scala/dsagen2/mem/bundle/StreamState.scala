package dsagen2.mem.bundle

import chisel3._
import dsagen2.top.config.DSAFixedConfig._

/** The State of Stream Bundle, by default it is a 6-bit Enumeration Bundle which encodes:
  * (LSB)
  *    1. Start of Stream
  *       2. End of Stream
  *       3. Start of 1D stream
  *       4. End of 1D stream
  *       5. Start of 2D stream
  *       6. End of 2D stream
  *       TODO: we should make it depends on the stream pattern supported by DMA, SPM, REC in the future
  */
class StreamState extends Bundle {
  /* ---------- Hardware Fields ---------- */
  val End2D:    Bool = Bool() // End of Stream 2D
  val Start2D:  Bool = Bool() // Start of Stream 1D
  val End1D:    Bool = Bool() // End of Stream 1D
  val Start1D:  Bool = Bool() // Start of Stream 1D
  val EndStr:   Bool = Bool() // End of Stream
  val StartStr: Bool = Bool() // Start of Stream

  // Mask AND
  def mask(start: Bool, end: Bool): StreamState = {
    val newState: StreamState = WireInit(0.U.asTypeOf(new StreamState))
    val mE1d:     Bool = End1D && end
    val mS1d:     Bool = Start1D && start
    newState.End1D := mE1d
    newState.Start1D := mS1d
    newState.End2D := End2D && mE1d
    newState.Start2D := Start2D & mS1d
    newState.EndStr := EndStr && mE1d
    newState.StartStr := StartStr && mS1d
    newState
  }

  // Combined with padding, to see whether it is padding moment
  def padNow(padding: UInt): (Bool, Bool) = {
    val padEnabled: Bool = padding =/= noPad
    val padZero: Bool =
      (EndStr && padding === padZeroStrEnd) || (End1D && padding === padZero1DEnd) || (End2D && padding === padZero2DEnd)
    val padOff: Bool =
      (EndStr && padding === padOffStrEnd) || (End1D && padding === padOff1DEnd) || (End2D && padding === padOff2DEnd)
    (padEnabled && padZero, padEnabled && padOff)
  }
}

object StreamState {
  def OR(sta1: StreamState, sta2: StreamState): StreamState = (sta1.asUInt() | sta2.asUInt()).asTypeOf(new StreamState)
}
