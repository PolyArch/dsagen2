package dsagen2.top.config.operation

import chisel3.util.isPow2

import scala.language.implicitConversions

object DataType extends Enumeration {
  // Convert String abbreviation to Data Type
  def abbr2DataType(str: String): DsaDataType = {
    str match {
      case "UI8"  => UnsignedInt8
      case "UI16" => UnsignedInt16
      case "UI32" => UnsignedInt32
      case "UI64" => UnsignedInt64
      case "SI8"  => SignedInt8
      case "SI16" => SignedInt16
      case "SI32" => SignedInt32
      case "SI64" => SignedInt64
      case "H16"  => Half16
      case "F32"  => Float32
      case "D64"  => Double64
      case err    => require(requirement = false, s"Unknown data type abbreviation $err"); UnsignedInt8
    }
  }

  // Implicit convert from value to DsaDataType case class
  implicit def value2dataType(v: Value): DsaDataType = v.asInstanceOf[DsaDataType]

  // Implicit convert from String to Data Type
  implicit def string2dataType(str: String): DsaDataType = {
    val filterResult: ValueSet = values.filter(v => v.str == str)
    if (filterResult.isEmpty) {
      require(requirement = false, s"Cannot find datatype whose name is $str")
      throw new IllegalArgumentException
    } else {
      filterResult.head
    }

  }

  // Enumeration Class
  protected[dsagen2] case class DsaDataType(str: String, compBits: Int, unitBits: Int) extends super.Val {
    require(isPow2(compBits), s"Total bits of $str is not power of 2")
    require(isPow2(unitBits), s"Unit bits of $str is not power of 2")

    def vecWidth: Int = compBits / unitBits

    def isDecomposable: Boolean = vecWidth > 1

    def isUnsigned: Boolean = str.startsWith("U")

    def isSigned: Boolean = !isUnsigned

    def isFixed: Boolean = isUnsigned || str.startsWith("I")

    def isFloat: Boolean = !isFixed
  }

  // Unsigned Integer
  val UnsignedInt8:    DsaDataType = DsaDataType("U8", 8, 8)
  val UnsignedInt8x2:  DsaDataType = DsaDataType("U8x2", 8 * 2, 8)
  val UnsignedInt8x4:  DsaDataType = DsaDataType("U8x4", 8 * 4, 8)
  val UnsignedInt8x8:  DsaDataType = DsaDataType("U8x8", 8 * 8, 8)
  val UnsignedInt16:   DsaDataType = DsaDataType("U16", 16, 16)
  val UnsignedInt16x2: DsaDataType = DsaDataType("U16x2", 16 * 2, 16)
  val UnsignedInt16x4: DsaDataType = DsaDataType("U16x4", 16 * 4, 16)
  val UnsignedInt16x8: DsaDataType = DsaDataType("U16x8", 16 * 8, 16)
  val UnsignedInt32:   DsaDataType = DsaDataType("U32", 32, 32)
  val UnsignedInt32x2: DsaDataType = DsaDataType("U32x2", 32 * 2, 32)
  val UnsignedInt32x4: DsaDataType = DsaDataType("U32x4", 32 * 4, 32)
  val UnsignedInt32x8: DsaDataType = DsaDataType("U32x8", 32 * 8, 32)
  val UnsignedInt64:   DsaDataType = DsaDataType("U64", 64, 64)
  val UnsignedInt64x2: DsaDataType = DsaDataType("U64x2", 64 * 2, 64)
  val UnsignedInt64x4: DsaDataType = DsaDataType("U64x4", 64 * 4, 64)
  val UnsignedInt64x8: DsaDataType = DsaDataType("U64x8", 64 * 8, 64)

  // Signed Integer
  val SignedInt8:    DsaDataType = DsaDataType("I8", 8, 8)
  val SignedInt8x2:  DsaDataType = DsaDataType("I8x2", 8 * 2, 8)
  val SignedInt8x4:  DsaDataType = DsaDataType("I8x4", 8 * 4, 8)
  val SignedInt8x8:  DsaDataType = DsaDataType("I8x8", 8 * 8, 8)
  val SignedInt16:   DsaDataType = DsaDataType("I16", 16, 16)
  val SignedInt16x2: DsaDataType = DsaDataType("I16x2", 16 * 2, 16)
  val SignedInt16x4: DsaDataType = DsaDataType("I16x4", 16 * 4, 16)
  val SignedInt16x8: DsaDataType = DsaDataType("I16x8", 16 * 8, 16)
  val SignedInt32:   DsaDataType = DsaDataType("I32", 32, 32)
  val SignedInt32x2: DsaDataType = DsaDataType("I32x2", 32 * 2, 32)
  val SignedInt32x4: DsaDataType = DsaDataType("I32x4", 32 * 4, 32)
  val SignedInt32x8: DsaDataType = DsaDataType("I32x8", 32 * 8, 32)
  val SignedInt64:   DsaDataType = DsaDataType("I64", 64, 64)
  val SignedInt64x2: DsaDataType = DsaDataType("I64x2", 64 * 2, 64)
  val SignedInt64x4: DsaDataType = DsaDataType("I64x4", 64 * 4, 64)
  val SignedInt64x8: DsaDataType = DsaDataType("I64x8", 64 * 8, 64)

  // Floating Point
  //  val Mini8:      DsaDataType =     DsaDataType("Mini8",      8,    8) //TODO: berkeley-float is not working for 8bit
  //  val Mini8x2:    DsaDataType =     DsaDataType("Mini8x2",    8*2,  8)
  //  val Mini8x4:    DsaDataType =     DsaDataType("Mini8x4",    8*4,  8)
  //  val Mini8x8:    DsaDataType =     DsaDataType("Mini8x8",    8*8,  8)
  val Half16:     DsaDataType = DsaDataType("H16", 16, 16)
  val Half16x2:   DsaDataType = DsaDataType("H16x2", 16 * 2, 16)
  val Half16x4:   DsaDataType = DsaDataType("H16x4", 16 * 4, 16)
  val Half16x8:   DsaDataType = DsaDataType("H16x8", 16 * 8, 16)
  val Float32:    DsaDataType = DsaDataType("F32", 32, 32)
  val Float32x2:  DsaDataType = DsaDataType("F32x2", 32 * 2, 32)
  val Float32x4:  DsaDataType = DsaDataType("F32x4", 32 * 4, 32)
  val Float32x8:  DsaDataType = DsaDataType("F32x8", 32 * 8, 32)
  val Double64:   DsaDataType = DsaDataType("D64", 64, 64)
  val Double64x2: DsaDataType = DsaDataType("D64x2", 64 * 2, 64)
  val Double64x4: DsaDataType = DsaDataType("D64x4", 64 * 4, 64)
  val Double64x8: DsaDataType = DsaDataType("D64x8", 64 * 8, 64) // 512-bit, widest vector operation for now

  // Unsigned Group
  val UnsignedNondecompGroup: Set[DsaDataType] = Set(UnsignedInt8, UnsignedInt16, UnsignedInt32, UnsignedInt64)
  val UnsignedDecomposableGroup: Set[DsaDataType] =
    this.values.filter(t => t.isUnsigned && t.isDecomposable).map(value2dataType)
  // Signed Group
  val SignedNondecompGroup: Set[DsaDataType] = Set(SignedInt8, SignedInt16, SignedInt32, SignedInt64)
  val SignedDecomposableGroup: Set[DsaDataType] =
    this.values.filter(t => t.isSigned && t.isFixed && t.isDecomposable).map(value2dataType)
  // Float Group
  val FloatNondecompGroup: Set[DsaDataType] = Set( /*Mini8, */ Half16, Float32, Double64)
  val FloatDecomposableGroup: Set[DsaDataType] =
    this.values.filter(t => t.isFloat && t.isDecomposable).map(value2dataType)
  val FloatGroup: Set[DsaDataType] = FloatNondecompGroup ++ FloatDecomposableGroup
  // Fixed Group
  val FixedNondecompGroup:    Set[DsaDataType] = UnsignedNondecompGroup ++ SignedNondecompGroup
  val FixedDecomposableGroup: Set[DsaDataType] = UnsignedDecomposableGroup ++ SignedDecomposableGroup
  val FixedGroup:             Set[DsaDataType] = FixedNondecompGroup ++ FixedDecomposableGroup
  // All Group
  val AllNondecompGroup:    Set[DsaDataType] = FloatNondecompGroup ++ FixedNondecompGroup
  val AllDecomposableGroup: Set[DsaDataType] = FixedDecomposableGroup ++ FloatDecomposableGroup
  val AllGroup:             Set[DsaDataType] = AllNondecompGroup ++ AllDecomposableGroup
}
