package dsagen2.sync.config

import chisel3.util.isPow2
import dsagen2.top.diplomacy.WithNodeIDParameters
import dsagen2.util.JSONParsableParameters

/** The basic node parameter of vector port
  * Input vector port and output vector port has its own properties:
  * input vector port can: state, repeat, broadcast stream
  * output vector port can: schedule, reorder stream, etc.
  * But they still share some common properties like vector width and depth, this trait cannot be used directly
  * it has to be used via IVP and OVP Node Parameters
  */
trait VPNodeParameters extends JSONParsableParameters with WithNodeIDParameters {

  /* Basic parameter of an vector port parameters */

  // data bits of each element of port
  // val unitBits : Int, although I know it is likely to be byte, but I think it should be determined by mem and comp

  // number of element of vector port can store in byte
  // because the granularity of memory and compute is different, so byte is used
  val depthByte: Int

  // Utility: direction of vector port
  def isInput: Boolean

  // Utility: Check the direction of vector port
  def isOutput: Boolean = !isInput

  // Sanity check: the depth of vector port should be power of two and positive
  // Depth = -1 means let the hardware generator decide
  require(
    depthByte == -1 || isPow2(depthByte) && depthByte > 0,
    s"The depth of vector port should be power of 2 and positive but it is $depthByte"
  )
}
