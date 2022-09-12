package dsagen2.top.config.enumeration

import chisel3.{Bool, Wire}
import dsagen2.top.bundle.{CanBeEncoded, EnumBundle}

import scala.collection.Set

object Predication extends Enumeration with CanBeEncoded {
  type Predication = Value
  override val encMethod: EnumEncodeMethod.Value = EnumEncodeMethod.numEncode
  override val trimmable: Boolean = false
  override val isOneHot:  Boolean = false

  ////////////////////////////////////////
  ///////  Triggered Architecture  ///////
  ////////////////////////////////////////
  val PredicateOn:  Predication = Value("PredicateOn")
  val PredicateOff: Predication = Value("PredicateOff")

  // Return the full set of predication enumeration
  def predicateSet: Set[Enumeration#Value] = Set(PredicateOn, PredicateOff)

  // Filter out the predicate enumeration in the Set of enumeration
  def getPredicate(potentialSet: Set[Enumeration#Value]): Set[Enumeration#Value] =
    potentialSet.filter { enum => enum.isInstanceOf[Predication] }

  def merge(source0: EnumBundle, source1: EnumBundle): EnumBundle = {
    // First, two input EnumBundle should be bundle that encodes Predicate
    require(source0.enumGroup.getOrElse(this) == this, s"Bundle is not encoding $this")
    require(source1.enumGroup.getOrElse(this) == this, s"Bundle is not encoding $this")
    // Since predication is either on or off, so it should be one bit or zero bit
    val mergedWire: EnumBundle = Wire(
      new EnumBundle(
        source0.enumerations ++ source1.enumerations,
        EnumEncodeMethod.numEncode,
        supportInvalid = false,
        isOneHot = false
      )
    )
    // Build the merged bits
    val mergePredicate: Option[Bool] = (source0.enumHardwareField, source1.enumHardwareField) match {
      case (Some(predicate0), Some(predicate1)) =>
        require(predicate0.getWidth == 1 && predicate1.getWidth == 1, s"Why source predicate is more than one bit")
        Some(predicate0.asBool() && predicate1.asBool())
      case (Some(predicate0), None) =>
        require(predicate0.getWidth == 1, s"predicate width is not 1-bit")
        Some(predicate0.asBool())
      case (None, Some(predicate1)) =>
        require(predicate1.getWidth == 1, s"predicate width is not 1-bit")
        Some(predicate1.asBool())
      case (None, None) => None
    }
    // Check the merged Wire contain both predication on and off
    require(
      mergedWire.enumerations.equals(predicateSet) || mergedWire.enumerations.isEmpty,
      s"the merged predication wire needs to contain predication on and off or just None wire"
    )
    // Connect the merged bits to merged wire
    (mergedWire.enumHardwareField, mergePredicate) match {
      case (Some(predicateWire), Some(predicateBit)) =>
        require(predicateWire.getWidth == predicateBit.getWidth, s"Bit width is not the same")
        predicateWire := predicateBit.asUInt()
      case (None, None) =>
      case _            => require(requirement = false, s"I don't think this case will exist")
    }
    // Return bundle
    mergedWire
  }
}
