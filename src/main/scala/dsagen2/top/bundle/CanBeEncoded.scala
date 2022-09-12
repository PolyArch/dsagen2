package dsagen2.top.bundle

import dsagen2.top.config.enumeration.EnumEncodeMethod

// This trait should only be inherited by class Enumeration
trait CanBeEncoded {
  this: Enumeration =>

  // The encoding method
  val encMethod: EnumEncodeMethod.Value

  // For any given trimmable Enumeration set, the EnumBundle can only support a subnet of the Enumeration,
  // which means that such EnumBundle should support Invalid, since two EnumBundle with different set of
  // enumerations can be connected together
  val trimmable: Boolean

  // Whether this Enumeration is one hot encoded
  val isOneHot: Boolean

  // For all can be encoded enumeration, they need to decide how two EnumBundle should be merged
  def merge(source0: EnumBundle, source1: EnumBundle): EnumBundle
}
