package dsagen2.top.config.enumeration

// Encoding Method
object EnumEncodeMethod extends Enumeration {
  type EnumEncodeMethod = Value
  // Use number to encode the enumeration, can only represent one status at a time, use less bits
  val numEncode: EnumEncodeMethod = Value("NumberEncoded")
  // Use one bit to encode the enumeration, multiple status can be represented at a time, use more bits
  val maskEncode: EnumEncodeMethod = Value("MaskEncoded")
}
