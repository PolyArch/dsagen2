package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._

/** This is the trait that should be inherited by all Config Port Bundle, by calling fieldBitsMap, you can get
  * Name -> (Bits, Defined) mapping
  */
trait CompConfigBitstream extends Bundle {
  val bitStreamEnc: Seq[(String, Seq[Int], Boolean)]
  implicit val p:   Parameters

  /** Convert Field High Low Position to #bits, with defined/not
    *
    * @return Map: FieldName -> (#FieldBits, FieldDefined)
    */
  def fieldBitsMap: Map[String, (Int, Boolean)] = bitStreamEnc.map { case (str, ints, defined) =>
    val high: Int = ints.head
    val low:  Int = ints(1)
    val bits: Int = if (defined) high - low + 1 else 0
    str -> (bits, defined)
  }.toMap
}
