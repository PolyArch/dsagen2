package dsagen2.top.config.enumeration

// The special encoding state of Enumeration
object SpecialEncodeState extends Enumeration {
  type SpecialEncode = Value
  // Invalid Encoding, meaning that upstream provides some encoding but this bundle does not understand
  val invalidState: SpecialEncode = Value("InvalidEncode")
}
