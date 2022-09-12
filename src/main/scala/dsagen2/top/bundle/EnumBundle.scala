package dsagen2.top.bundle

import chisel3._
import chisel3.util._
import dsagen2.top.config.enumeration
import dsagen2.top.config.enumeration.{EnumEncodeMethod, SpecialEncodeState}
import dsagen2.util.EnumerationUtil._

import scala.collection.Set
import scala.collection.mutable.ListBuffer

/** The Bundle Type to support Enumeration, by default, it is mask encoded bundle without invalid state and not one hot
  *
  * @param enumerations   The enumeration set that can be represented by this bundle
  * @param encMethod      The encoding method of this bundle
  * @param supportInvalid whether this bundle will be connected to another EnumBundle that contains something it does not have
  * @param isOneHot       if this bundle is mask encoded, whether is one hot bundle, which will enforce some assertions
  */
class EnumBundle(
  val enumerations:   Set[Enumeration#Value],
  val encMethod:      EnumEncodeMethod.Value = EnumEncodeMethod.maskEncode,
  val supportInvalid: Boolean = false,
  val isOneHot:       Boolean = false)
    extends Bundle {
  // An internal field that keep track of every bit of mask encoded EnumBundle
  private val assigned: Array[Boolean] = encMethod match {
    case dsagen2.top.config.enumeration.EnumEncodeMethod.numEncode =>
      Array.empty
    case dsagen2.top.config.enumeration.EnumEncodeMethod.maskEncode =>
      Array.fill(enumSeq.size)(false)
  }

  // Utility to get the encoding method fast
  def isNumEncode: Boolean = encMethod == EnumEncodeMethod.numEncode

  def isMaskEncode: Boolean = encMethod == EnumEncodeMethod.maskEncode

  def isOneHotEncode: Boolean = isMaskEncode && isOneHot

  // Get enumeration group
  def enumGroup: Option[Enumeration with CanBeEncoded] =
    if (enumerations.nonEmpty) getEnumGroups(enumerations).head match {
      case group: Enumeration with CanBeEncoded => Some(group)
      case other =>
        require(requirement = false, s"Enumeration $other can not be encoded")
        None
    }
    else None

  // Sanity Check: for one EnumBundle, the enumeration it represents should belong to one group
  require(
    getEnumGroups(enumerations).size <= 1,
    s"The enumeration of one EnumBundle should belongs to one Group," +
      s"but this EnumBundle contains $enumerations"
  )

  // Sanity Check: Only Mask Encoded EnumBundle can be one hot encoded
  if (isOneHot) require(encMethod == EnumEncodeMethod.maskEncode, s"EnumBundle is one hot, but not mask encoded")

  // Get the unique sequence of enumeration, ordering based on hashcode, so it will be unique
  def enumSeq: Seq[Enumeration#Value] = {
    // Prefix Sequence of State
    val preEnums: ListBuffer[Enumeration#Value] = new ListBuffer()
    // Postfix Sequence of State
    val postEnums: ListBuffer[Enumeration#Value] = new ListBuffer()
    // Prepend invalid state if the invalid State is supported, prepend means lower bits will be used to encode it
    if (supportInvalid && enumerations.nonEmpty) preEnums += SpecialEncodeState.invalidState
    // TODO: append some states to the post enumerations, which mean use the higher bits to encode it

    // Sequential the enumerations set and sort it based on hashcode
    val el: Seq[Enumeration#Value] = enumerations.toSeq.sortWith(_.hashCode() < _.hashCode())
    // Return the constructed sequence
    preEnums.result() ++ el ++ postEnums.result()
  }

  // Assign a number for each of the enumeration, zero (0) and one (b'00001) will be used as invalid signal by default
  // Return: Set of enumeration with its assigned number, and number of bits to encode it
  def enumMap: Map[Enumeration#Value, BigInt] = {
    // Generate the enumeration set with assigned number
    enumSeq.zipWithIndex.map {
      // Invalid bit at the LSB or zero
      case (enumeration, idx) =>
        encMethod match {
          // Enumeration is encoded as number in order
          case EnumEncodeMethod.numEncode => (enumeration, BigInt(idx))
          // Enumeration is encoded as bit from LSB to MSB, both mask and one hot are encoded in this way
          case EnumEncodeMethod.maskEncode => (enumeration, BigInt(1) << idx)
        }
    }.toMap
  }

  // Calculate how many bits needed to encode this field
  def enumBits: Int =
    if (enumerations.nonEmpty) {
      encMethod match {
        // Number encoded
        case enumeration.EnumEncodeMethod.numEncode => log2Ceil(enumSeq.size)
        // Mask encoded
        case enumeration.EnumEncodeMethod.maskEncode => enumSeq.size
      }
    } else 0

  // Find the index of this enumeration
  def index(e: Enumeration#Value): Int =
    if (enumerations.nonEmpty) enumSeq.indexOf(e) else -1

  // Return sink only set, common set, source only set
  def onlyAndCommon(sink: EnumBundle): (Seq[Enumeration#Value], Seq[Enumeration#Value], Seq[Enumeration#Value]) = {
    (
      sink.enumSeq.diff(this.enumSeq), // sink only Enumeration, will be always false
      sink.enumSeq.intersect(this.enumSeq), // common value enumeration, depends on the source value
      this.enumSeq.diff(sink.enumSeq)
    ) // source only enumeration, will result in invalid signal
  }

  // Convert the current enumeration field to another set mask encoded hardware
  def toMaskEncoded(sink: EnumBundle): Option[UInt] = {
    // Require that bundle is mask encoded, other wise why you want to call this function?
    require(sink.encMethod == EnumEncodeMethod.maskEncode, s"This bundle is not mask encoded why you call this func?")
    // Calculate the sinkOnly, common, sourceOnly set
    val (sinkOnly, commonSet, sourceOnly) = onlyAndCommon(sink)
    // Judge between the current encoding method
    encMethod match {
      // From number encoded (this,source) to mask encoded
      case enumeration.EnumEncodeMethod.numEncode =>
        sink.enumHardwareField match {
          case Some(sinkField) =>
            // Calculate the valid signal for sink
            val sinkSignalSeq: Seq[Bool] = sink.enumSeq.map {
              case SpecialEncodeState.invalidState =>
                // If the source field match any source only enumeration, it will be invalid for sink
                val matchSourceOnlyOrInvalid: Seq[Bool] = (SpecialEncodeState.invalidState +: sourceOnly).map { o =>
                  if (index(o) >= 0) {
                    index(o).U === enumHardwareField.get
                  } else {
                    false.B // like source does not support invalidState
                  }
                }
                VecInit(matchSourceOnlyOrInvalid).asUInt().orR()
              case sinkEnum =>
                // The rest of enumeration will be treated normally
                if (sinkOnly.contains(sinkEnum)) {
                  false.B
                } else if (commonSet.contains(sinkEnum)) {
                  val thisNum: Int = index(sinkEnum)
                  enumHardwareField.get === thisNum.U
                } else {
                  require(requirement = false, s"This by definition can not happen")
                  false.B
                }
            }
            // The bit width should match
            require(sinkField.getWidth == sinkSignalSeq.length, s"Bit width mismatch")
            // Convert the sequence of Bool to UInt
            if (sinkSignalSeq.nonEmpty) Some(VecInit(sinkSignalSeq).asUInt()) else None
          case None => None
        }
      // From mask encoded (this, source) to mask encoded
      case enumeration.EnumEncodeMethod.maskEncode =>
        sink.enumHardwareField match {
          case Some(sinkField) =>
            // Calculate the valid signal for sink
            val sinkSignalSeq: Seq[Bool] = sink.enumSeq.map {
              case SpecialEncodeState.invalidState =>
                val matchSourceOnlyOrInvalid = (SpecialEncodeState.invalidState +: sourceOnly).map { o =>
                  if (index(o) >= 0) {
                    enumHardwareField.get.apply(index(o))
                  } else {
                    false.B
                  }
                }
                VecInit(matchSourceOnlyOrInvalid).asUInt().orR()
              case sinkEnum =>
                // The rest of enumeration will be treated normally
                if (sinkOnly.contains(sinkEnum)) {
                  false.B
                } else if (commonSet.contains(sinkEnum)) {
                  enumHardwareField.get.apply(index(sinkEnum))
                } else {
                  require(requirement = false, "this case should never happen")
                  false.B
                }
            }
            // The bit width should match
            require(sinkField.getWidth == sinkSignalSeq.length, s"Bit width mismatch")
            // Convert the sequence of bool to UInt
            if (sinkSignalSeq.nonEmpty) Some(VecInit(sinkSignalSeq).asUInt()) else None
          case None => None
        }
    }
  }

  // Convert the current enumeration field to another set number encoded hardware
  def toNumEncoded(sink: EnumBundle): Option[UInt] = {
    // Require sink bundle is number encoded, otherwise why you want to call this function?
    require(sink.encMethod == EnumEncodeMethod.numEncode, "This bundle is not number encoded, why you cal this func?")
    // Judge between the current encoding method
    val converted: Option[UInt] = encMethod match {

      // From number encoded to number encoded
      case enumeration.EnumEncodeMethod.numEncode =>
        // Generate the mapping from source to sink
        val lut: Seq[(UInt, UInt)] = enumSeq.map { sourceEnum =>
          // Get the source number
          val sourceNum: Int = index(sourceEnum)
          // Get the sink number
          val sinkNum: Int = {
            if (sink.enumSeq.contains(sourceEnum)) {
              sink.index(sourceEnum)
            } else if (sink.enumSeq.contains(SpecialEncodeState.invalidState)) {
              sink.index(SpecialEncodeState.invalidState)
            } else {
              require(requirement = false, s"Sink Bundle does not support $sourceEnum, and it doesn't have invalid")
              0
            }
          }
          // Calculate the hardware result
          val sourceUInt = if (enumBits > 0) sourceNum.U(enumBits.W) else 0.U(1.W)
          val sinkUInt = if (sink.enumBits > 0) sinkNum.U(sink.enumBits.W) else 0.U(1.W)
          (sourceUInt, sinkUInt)
        }
        // Return based on sink Field
        sink.enumHardwareField match {
          case Some(sinkField) =>
            enumHardwareField match {
              // Source and sink both has fields
              case Some(sourceField) => Some(MuxLookup(sourceField, 0.U, lut))
              // Source has nothing, sink has something, which means that sink will always be invalid; if sink does not
              // support invalid, then we just return None
              case None =>
                // Get the index of invalid
                val invalidNumInSink: Int = sink.index(SpecialEncodeState.invalidState)
                // Return
                if (invalidNumInSink >= 0) Some(invalidNumInSink.U) else None
            }
          case None => None
        }

      // From mask encoded to number encoded
      case enumeration.EnumEncodeMethod.maskEncode =>
        // Generate the mapping from source to sink
        val lut: Seq[(UInt, UInt)] = enumSeq.map { sourceEnum =>
          // Get the source encoding
          val sourceUInt: UInt = enumMap(sourceEnum).U(enumBits.W)
          // Get the sink encoding
          val sinkUInt: UInt = {
            if (sink.enumSeq.contains(sourceEnum)) {
              sink.enumMap(sourceEnum).U(sink.enumBits.W)
            } else if (sink.enumSeq.contains(SpecialEncodeState.invalidState)) {
              sink.enumMap(SpecialEncodeState.invalidState).U(sink.enumBits.W)
            } else {
              require(requirement = false, s"Sink doesn't support invalid, nor support $sourceEnum")
              0.U
            }
          }
          (sourceUInt, sinkUInt)
        }
        // Return based on the existence of field
        sink.enumHardwareField match {
          case Some(sinkField) =>
            enumHardwareField match {
              case Some(sourceField) => Some(MuxLookup(sourceField, 0.U, lut))
              case None              =>
                // Get the index of invalid
                val invalidNumInSink: Int = sink.index(SpecialEncodeState.invalidState)
                // Return
                if (invalidNumInSink >= 0) Some(invalidNumInSink.U) else None
            }
          case None => None
        }
    }
    // Make sure that converted UInt is same wide as the sink
    (sink.enumHardwareField, converted) match {
      case (Some(sinkField), Some(convertField)) =>
        require(sinkField.getWidth == convertField.getWidth, s"Converted EnumBundle should be same width as sink")
      case _ =>
    }
    // Return Converted
    converted
  }

  // The Actual Hardware
  val enumHardwareField: Option[UInt] = if (enumBits > 0) Some(UInt(enumBits.W)) else None

  // Sanity Check: If this EnumBundle is one hot, then do the one hot check
  if (isOneHot) {
    encMethod match {
      case enumeration.EnumEncodeMethod.numEncode =>
        // If the encoding method is number encoded, call it isOneHot has no meaning
        require(requirement = false, s"This is number encoded enumeration bundle, which is not related to One Hot")
        false.B
      case enumeration.EnumEncodeMethod.maskEncode =>
        // For One Hot Bundle, the legal state will be either all bits are false, or just one bit is enabled
        enumHardwareField match {
          case Some(eField) if eField.getWidth > 1 =>
          /* TODO : please bring the one hot check for exception
          assert(PopCount(Wire(enumHardwareField.getOrElse(0.U))) <= 1.U, s"$this EnumBundle is not One Hot",
            enumHardwareField.getOrElse(0.U))*/
          case _ =>
        }
    }
  }

  // Check whether this EnumBundle's state is equal to provided, there is the read only, cannot be assigned
  def is(eoi: Enumeration#Value): Bool = {
    // Check whether the provided Enumeration#Value belongs to the Enumeration Group of this bundle
    val eoiGroup: Enumeration = getEnumGroups(Set(eoi)).head
    if (enumerations.nonEmpty) {
      require(
        eoiGroup == enumGroup.get,
        s"Enumeration of Interest $eoi's group $eoiGroup is not the enumeration" +
          s"group (${enumGroup.get}) of this Bundle, accessing `is` function is meaningless"
      )
    } else {
      require(requirement = false, s"This EnumBundle represent Empty Set, accessing `is` function is meaningless")
    }
    // Only when the enumeration is defined in hardware, should we access the hardware
    if (enumSeq.contains(eoi)) {
      // Get the index
      val enumIdx: Int = index(eoi)
      require(enumIdx >= 0, s"If it is contained, why it will be not found (index < 0)")
      // return Bool based on encMethod
      encMethod match {
        case enumeration.EnumEncodeMethod.numEncode =>
          enumHardwareField.getOrElse(0.U) === enumIdx.U
        case enumeration.EnumEncodeMethod.maskEncode =>
          enumHardwareField.getOrElse(0.U(1.W)).apply(enumIdx)
      }
    } else {
      // Otherwise, since the Enumeration is not supported, just false.B
      false.B
    }
  }
}

object EnumBundle {

  /** Connect one enumeration bundle to another one, if source is maskEncoded bundle. Multi-enable bit will result in
    * invalid
    */
  def connect(sink: EnumBundle, source: EnumBundle): Unit = {
    val source2sink: Option[UInt] = sink.encMethod match {
      case enumeration.EnumEncodeMethod.numEncode  => source.toNumEncoded(sink)
      case enumeration.EnumEncodeMethod.maskEncode => source.toMaskEncoded(sink)
    }
    (sink.enumHardwareField, source2sink) match {
      case (Some(sinkField), Some(convertedSource)) =>
        require(
          sinkField.getWidth == convertedSource.getWidth,
          s"Converted source's width (${convertedSource.getWidth}) is different from " +
            s"sink's width (${sinkField.getWidth})"
        )
        sinkField := convertedSource
      case (Some(sinkField), None) =>
        sinkField := 0.U
      case _ =>
    }
  }

  /** Wrapper of merging two EnumBundle types to one EnumBundle type. The actual merging operation is determined by
    * the Encoded Enumeration
    *
    * @param source0 The first source of the EnumBundle
    * @param source1 The second source of the EnumBundle
    * @return The merged EnumBundle of provided two EnumBundle
    */
  def merge(source0: EnumBundle, source1: EnumBundle): EnumBundle = {
    (source0.enumHardwareField, source1.enumHardwareField) match {
      case (Some(_), Some(_)) =>
        // both EnumBundles are defined, make sure that they belong to the same Enumeration
        require(
          source0.enumGroup.get == source1.enumGroup.get,
          s"Two EnumBundles with different Enumeration group: " +
            s"${source0.enumGroup.get} and ${source1.enumGroup.get} cannot be merged"
        )
        // Get the Enumeration
        val enumGroup: Enumeration with CanBeEncoded = source0.enumGroup match {
          case Some(value) => value
          case None        => throw new NullPointerException
        }
        // Use the Enumeration specific merge to combine two EnumBundle
        enumGroup.merge(source0, source1)
      case (Some(_), None) => source0
      case (None, Some(_)) => source1
      case _               => Wire(new EnumBundle(Set.empty))
    }
  }
}
