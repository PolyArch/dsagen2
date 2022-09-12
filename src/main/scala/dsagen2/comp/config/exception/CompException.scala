package dsagen2.comp.config.exception

import chisel3._
import dsagen2.top.bundle.{CanBeEncoded, EnumBundle}
import dsagen2.top.config.enumeration.EnumEncodeMethod

import scala.collection.Set
import scala.language.implicitConversions

object CompException extends Enumeration with CanBeEncoded {

  // Computation Exception can of course be encoded by some wire

  // The encoding method
  val encMethod: EnumEncodeMethod.Value = EnumEncodeMethod.maskEncode
  val trimmable: Boolean = true
  val isOneHot:  Boolean = false

  implicit def EnumToException(o: Enumeration#Value): ComputationException =
    o.asInstanceOf[ComputationException]

  implicit def EnumSet2ExceptionSet(se: Set[Enumeration#Value]): Set[ComputationException] =
    se.map(EnumToException)

  implicit def Exception2Enum(exp: ComputationException): Enumeration#Value =
    exp.asInstanceOf[Enumeration#Value]

  implicit def ExceptionSet2EnumSet(es: Set[ComputationException]): Set[Enumeration#Value] =
    es.map(Exception2Enum)

  implicit def stringToException(str: String): ComputationException = {
    if (values.exists(e => e.toString == str)) {
      val finds = values.filter(p => p.toString == str)
      require(finds.size == 1, s"You find more than one exception?")
      finds.head
    } else {
      require(requirement = false, s"No $str exception has been defined")
      FixedDividedByZero
    }
  }

  override def merge(source0: EnumBundle, source1: EnumBundle): EnumBundle = {

    // Create the wire with merged enumerations
    val mergedWire: EnumBundle = Wire(
      new EnumBundle(
        source0.enumerations ++ source1.enumerations, // the status of merged wire should be union set of sources
        EnumEncodeMethod.maskEncode,
        /* TODO: technically, computation exception should be number encoded
      but we are merging two exceptions here, so there are cases that two input exceptions are different, which means we
      will need to encode two state in the merged EnumBundle. That is why it is mask encoded. */
        supportInvalid = false, // if the merged enumeration is union, there is no case for invalid
        isOneHot = false
      )
    ) // cannot be one hot since two inputs can be different

    // Operate on both source and connect to the merge wire
    val mergeBits: Option[UInt] = {
      mergedWire.enumSeq match {
        case Nil => None
        case mEnumSeq: Seq[Enumeration#Value] =>
          val mergedBoolSeq: Seq[Bool] = mEnumSeq.map { mEnum =>
            source0.is(mEnum) || source1.is(mEnum) // Computation Exception merging is done via OR operation
          }
          Some(VecInit(mergedBoolSeq).asUInt())
      }
    }

    // Connect to the mergedWire
    (mergedWire.enumHardwareField, mergeBits) match {
      case (Some(wF), Some(mF)) =>
        require(
          wF.getWidth == mF.getWidth,
          s"Merged bits' width (${mF.getWidth}) is different from " +
            s"merged wire (${wF.getWidth})"
        )
        wF := mF
      case (None, None) =>
      case _ =>
        require(requirement = false, s"There should not be this case, ")
    }

    // return the merged EnumBundle
    mergedWire
  }

  // Case class definition
  type ComputationException = Value

  // MSB
  // Float Computation Exception
  val InvalidOperation:   ComputationException = Value("FloatInvalidOperation")
  val FloatDividedByZero: ComputationException = Value("FloatDividedByZero")
  val Overflow:           ComputationException = Value("FloatOverflow")
  val Underflow:          ComputationException = Value("FloatUnderflow")
  val Inexact:            ComputationException = Value("FloatInexact")

  // Fixed Computation Exception
  val FixedDividedByZero: ComputationException = Value("IntegerDividedByZero")
  val FixedOverflow:      ComputationException = Value("IntegerOverflow")
  // LSB

  // Exception Group

  // Floating Point Result Exception
  val floatException: Set[Enumeration#Value] = Set(InvalidOperation, FloatDividedByZero, Overflow, Underflow, Inexact)

  // Floating Conversion Exception
  val floatConversionException: Set[Enumeration#Value] = Set(InvalidOperation, Overflow, Inexact)
}
