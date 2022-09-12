package dsagen2.top.config.operation

import dsagen2.comp.config.exception.CompException.{ComputationException, FixedDividedByZero, FixedOverflow, floatException, _}
import dsagen2.top.config.operation.DataType._
import dsagen2.top.config.operation.OperDataType._
import dsagen2.top.config.operation.OperationGroup._

import scala.collection.Set
import scala.language.implicitConversions

object Operation extends Enumeration {
  // Implicit Convert to DsaOperation
  implicit def valueToDsaOperation(x: Value): DsaOperation = x.asInstanceOf[DsaOperation]

  implicit def stringToDsaOperation(str: String): DsaOperation = {
    if (this.values.map(_.str).contains(str)) {
      val filters: ValueSet = this.values.filter(x => x.str == str)
      require(filters.size == 1, s"You have more than one operations that share same String name $filters")
      filters.head
    } else {
      require(requirement = false, s"Operation $str is not found")
      FixedAdd
    }
  }

  // Case class of enumeration
  protected[dsagen2] case class DsaOperation(
    str:       String,
    opGroup:   DsaOperGroup,
    excpGroup: Set[ComputationException] = Set.empty)
      extends super.Val {

    // Sanity Check for DsaOperation, make sure Properties has its properties
    def sanity(): Unit = {
      // Require related properties are defined
      require(OperationNumOperandResult.isDefinedAt(this), s"You forget to set the properties of $str operation")
    }

    // Properties gather
    def numOperand: Int = {
      sanity()
      OperationNumOperandResult(this)._1
    }

    def numResult: Int = {
      sanity();
      OperationNumOperandResult(this)._2
    }

    // Convert to String
    override def toString: String = str

    // Check Operation Group Type
    def is(t: DsaOperGroup): Boolean = t == opGroup

    // Combine with datatype to form OperationDataType
    def +(dataType: DsaDataType): DsaOperDataType = {
      require(opGroup.applicableDTs(this).contains(dataType), s"$str 's group $opGroup does not support $dataType")
      DsaOperDataType(this, Some(dataType))
    }

    // bitwise operation to OperationDataType directly
    def toOpDataType: DsaOperDataType = {
      if (this.is(BitwiseOperation) || this.is(FloatConversion) || this.is(FixedConversion))
        DsaOperDataType(this, None)
      else {
        require(
          requirement = false,
          s"Only Bitwise/DataType Conversion operation can be convert to OperationDataType " +
            s"directly, this is $str in group $opGroup"
        )
        DsaOperDataType(this, None)
      }
    }

    // Return OperationDataType with all supported datatype
    def withAllDataType: Set[DsaOperDataType] = opGroup.applicableDTs(this) match {
      // Bitwise operation
      case m: Set[DsaDataType] if m.isEmpty =>
        require(this.is(BitwiseOperation), s"If it is not Bitwise, why support data type is empty")
        Set(DsaOperDataType(this, None))
      // No-bitwise operation
      case s: Set[DsaDataType] if s.nonEmpty => s.map(t => tuple2opDataType(this, t))
      // Failed
      case _ => require(requirement = false, "supported data type is problematic"); Set.empty
    }

    // Return Operation + DataType by providing the data type width
    def WithDataType(
      dataBits:     Int,
      unitBits:     Int,
      withoutGroup: Set[DsaOperGroup] = Set.empty
    ): Set[DsaOperDataType] = {
      withAllDataType.filter { case DsaOperDataType(operation, dataType, predefinedImpl) =>
        dataType match {
          case Some(dt) =>
            dt.compBits == dataBits && dt.unitBits == unitBits &&
              !withoutGroup.contains(operation.opGroup)
          case None => true // bitwise should be any data bits
        }
      }
    }

    // Return the exception set
    def getExceptions: Set[ComputationException] = excpGroup

    // Parse the string for those data Type conversion operation, return source/sink datatype
    def sourceSinkDT: (DsaDataType, DsaDataType, Boolean) = {
      if (this.is(FixedConversion) || this.is(FloatConversion)) {
        val source2sink: Array[DsaDataType] = str.split("To").map(DataType.abbr2DataType)
        require(
          source2sink.length == 2,
          s"Either cannot be converted, or is a pair, " +
            s"this is ${source2sink.mkString("Array(", ", ", ")")}"
        )
        val sourceDT = source2sink.head
        val sinkDT = source2sink(1)
        require(
          sourceDT.vecWidth == sinkDT.vecWidth && sinkDT.vecWidth == 1,
          s"The embedded data type in operation string should have nothing to do with vectorization"
        )
        (sourceDT, sinkDT, true)
      } else {
        (UnsignedInt8, UnsignedInt8, false) // not data type conversion operation
      }
    }
  }

  // Fixed Integer Operation (A = input[0], B = input[1], ...)
  val FixedAdd:      DsaOperation = DsaOperation("Add", FixedArithmetics) // A + B
  val FixedHLAdd:    DsaOperation = DsaOperation("HLAdd", FixedArithmetics) // B = A_high + A_low
  val FixedSub:      DsaOperation = DsaOperation("Sub", FixedArithmetics) // A - B
  val FixedMul:      DsaOperation = DsaOperation("Mul", FixedArithmetics, Set(FixedOverflow)) // A x B
  val FixedPMulPAdd: DsaOperation = DsaOperation("ppMulAdd", FixedArithmetics, Set(FixedOverflow)) // A x B + C
  val FixedPMulNAdd: DsaOperation = DsaOperation("pnMulAdd", FixedArithmetics, Set(FixedOverflow)) // A x B - C
  val FixedNMulPAdd: DsaOperation = DsaOperation("npMulAdd", FixedArithmetics, Set(FixedOverflow)) //-A x B + C
  val FixedNMulNAdd: DsaOperation = DsaOperation("nnMulAdd", FixedArithmetics, Set(FixedOverflow)) //-A x B - C
  val FixedDiv:      DsaOperation = DsaOperation("Div", FixedArithmetics, Set(FixedDividedByZero)) // A / B
  val FixedMod:      DsaOperation = DsaOperation("Mod", FixedArithmetics, Set(FixedDividedByZero)) // A % B
  val Concat:        DsaOperation = DsaOperation("Concat", FixedArithmetics) // (MSB) [A ; B] (LSB)
  val Select:        DsaOperation = DsaOperation("Select", FixedArithmetics) // if(C) A else B
  val LeftShift:     DsaOperation = DsaOperation("LShf", FixedArithmetics) // A << B
  val RightShift:    DsaOperation = DsaOperation("RShf", FixedArithmetics) // A >> B
  val Compare:       DsaOperation = DsaOperation("Comp", FixedArithmetics) // A comp B
  val FixedMax:      DsaOperation = DsaOperation("Max", FixedArithmetics) // A max B
  val FixedMin:      DsaOperation = DsaOperation("Min", FixedArithmetics) // A min B

  // Floating Point (for now using the berkeley hardfloat)
  // Standard:            Add/Sub/Mul/Div, Sqrt, Mod, Round, Compare (includes special value)
  // Berkeley Hardfloat:  Add, Mul, MulAdd, DivSqrt, Compare, Int2Float, Float2Int, Float2Float
  val FloatAdd:      DsaOperation = DsaOperation("FAdd", FloatArithmetics, floatException) // A + B
  val FloatSub:      DsaOperation = DsaOperation("FSub", FloatArithmetics, floatException) // A - B
  val FloatMul:      DsaOperation = DsaOperation("FMul", FloatArithmetics, floatException) // A * B
  val FloatPMulPAdd: DsaOperation = DsaOperation("FppMulAdd", FloatArithmetics, floatException) // A x B + C
  val FloatPMulNAdd: DsaOperation = DsaOperation("FpnMulAdd", FloatArithmetics, floatException) // A x B - C
  val FloatNMulPAdd: DsaOperation = DsaOperation("FnpMulAdd", FloatArithmetics, floatException) //-A x B + C
  val FloatNMulNAdd: DsaOperation = DsaOperation("FnnMulAdd", FloatArithmetics, floatException) //-A x B - C
  val FloatDiv:      DsaOperation = DsaOperation("FDiv", FloatArithmetics, floatException) // A / B
  val FloatSqrt:     DsaOperation = DsaOperation("FSqrt", FloatArithmetics, floatException) // sqrt(A)
  val FloatCompare:  DsaOperation = DsaOperation("FComp", FloatArithmetics, Set(InvalidOperation)) // A > B

  // Float to Float Convert
  val Half16ToFloat32:   DsaOperation = DsaOperation("H16ToF32", FloatConversion, floatException) // half -> float
  val Half16ToDouble64:  DsaOperation = DsaOperation("H16ToD64", FloatConversion, floatException) // half -> double
  val Float32ToHalf16:   DsaOperation = DsaOperation("F32ToH16", FloatConversion, floatException) // float -> half
  val Float32ToDouble64: DsaOperation = DsaOperation("F32ToD64", FloatConversion, floatException) // float -> double
  val Double64ToHalf16:  DsaOperation = DsaOperation("D64ToH16", FloatConversion, floatException) // double -> half
  val Double64ToFloat32: DsaOperation = DsaOperation("D64ToF32", FloatConversion, floatException) // double -> float

  // Unsigned Integer to Float Convert
  val UInt8ToHalf16:    DsaOperation = DsaOperation("UI8ToH16", FloatConversion, floatException) // UInt8 -> Half 16
  val UInt8ToFloat32:   DsaOperation = DsaOperation("UI8ToF32", FloatConversion, floatException) // UInt8 -> Float 32
  val UInt8ToDouble64:  DsaOperation = DsaOperation("UI8ToD64", FloatConversion, floatException) // UInt8 -> Double 64
  val UInt16ToHalf16:   DsaOperation = DsaOperation("UI16ToH16", FloatConversion, floatException) // UInt16 -> Half 16
  val UInt16ToFloat32:  DsaOperation = DsaOperation("UI16ToF32", FloatConversion, floatException) // UInt16 -> Float 32
  val UInt16ToDouble64: DsaOperation = DsaOperation("UI16ToD64", FloatConversion, floatException) // UInt16 -> Double 64
  val UInt32ToHalf16:   DsaOperation = DsaOperation("UI32ToH16", FloatConversion, floatException) // UInt32 -> Half 16
  val UInt32ToFloat32:  DsaOperation = DsaOperation("UI32ToF32", FloatConversion, floatException) // UInt32 -> Float 32
  val UInt32ToDouble64: DsaOperation = DsaOperation("UI32ToD64", FloatConversion, floatException) // UInt32 -> Double 64
  val UInt64ToHalf16:   DsaOperation = DsaOperation("UI64ToH16", FloatConversion, floatException) // UInt64 -> Half 16
  val UInt64ToFloat32:  DsaOperation = DsaOperation("UI64ToF32", FloatConversion, floatException) // UInt64 -> Float 32
  val UInt64ToDouble64: DsaOperation = DsaOperation("UI64ToD64", FloatConversion, floatException) // UInt64 -> Double 64

  // Signed Integer to Float Convert
  val SInt8ToHalf16:    DsaOperation = DsaOperation("SI8ToH16", FloatConversion, floatException) // SInt8 -> Half 16
  val SInt8ToFloat32:   DsaOperation = DsaOperation("SI8ToF32", FloatConversion, floatException) // SInt8 -> Float 32
  val SInt8ToDouble64:  DsaOperation = DsaOperation("SI8ToD64", FloatConversion, floatException) // SInt8 -> Double 64
  val SInt16ToHalf16:   DsaOperation = DsaOperation("SI16ToH16", FloatConversion, floatException) // SInt16 -> Half 16
  val SInt16ToFloat32:  DsaOperation = DsaOperation("SI16ToF32", FloatConversion, floatException) // SInt16 -> Float 32
  val SInt16ToDouble64: DsaOperation = DsaOperation("SI16ToD64", FloatConversion, floatException) // SInt16 -> Double 64
  val SInt32ToHalf16:   DsaOperation = DsaOperation("SI32ToH16", FloatConversion, floatException) // SInt32 -> Half 16
  val SInt32ToFloat32:  DsaOperation = DsaOperation("SI32ToF32", FloatConversion, floatException) // SInt32 -> Float 32
  val SInt32ToDouble64: DsaOperation = DsaOperation("SI32ToD64", FloatConversion, floatException) // SInt32 -> Double 64
  val SInt64ToHalf16:   DsaOperation = DsaOperation("SI64ToH16", FloatConversion, floatException) // SInt64 -> Half 16
  val SInt64ToFloat32:  DsaOperation = DsaOperation("SI64ToF32", FloatConversion, floatException) // SInt64 -> Float 32
  val SInt64ToDouble64: DsaOperation = DsaOperation("SI64ToD64", FloatConversion, floatException) // SInt64 -> Double 64

  // Float to Unsigned Convert
  val Half16ToUInt8: DsaOperation =
    DsaOperation("H16ToUI8", FloatConversion, floatConversionException) // Half16 -> UInt8
  val Half16ToUInt16: DsaOperation =
    DsaOperation("H16ToUI16", FloatConversion, floatConversionException) // Half16 -> UInt16
  val Half16ToUInt32: DsaOperation =
    DsaOperation("H16ToUI32", FloatConversion, floatConversionException) // Half16 -> UInt32
  val Half16ToUInt64: DsaOperation =
    DsaOperation("H16ToUI64", FloatConversion, floatConversionException) // Half16 -> UInt64
  val Float32ToUInt8: DsaOperation =
    DsaOperation("F32ToUI8", FloatConversion, floatConversionException) // Float32 -> UInt8
  val Float32ToUInt16: DsaOperation =
    DsaOperation("F32ToUI16", FloatConversion, floatConversionException) // Float32 -> UInt16
  val Float32ToUInt32: DsaOperation =
    DsaOperation("F32ToUI32", FloatConversion, floatConversionException) // Float32 -> UInt32
  val Float32ToUInt64: DsaOperation =
    DsaOperation("F32ToUI64", FloatConversion, floatConversionException) // Float32 -> UInt64
  val Double64ToUInt8: DsaOperation =
    DsaOperation("D64ToUI8", FloatConversion, floatConversionException) // Double64 -> UInt8
  val Double64ToUInt16: DsaOperation =
    DsaOperation("D64ToUI16", FloatConversion, floatConversionException) // Double64 -> UInt16
  val Double64ToUInt32: DsaOperation =
    DsaOperation("D64ToUI32", FloatConversion, floatConversionException) // Double64 -> UInt32
  val Double64ToUInt64: DsaOperation =
    DsaOperation("D64ToUI64", FloatConversion, floatConversionException) // Double64 -> UInt64

  // Float to Signed Convert
  val Half16ToSInt8: DsaOperation =
    DsaOperation("H16ToSI8", FloatConversion, floatConversionException) // Half16 -> SInt8
  val Half16ToSInt16: DsaOperation =
    DsaOperation("H16ToSI16", FloatConversion, floatConversionException) // Half16 -> SInt16
  val Half16ToSInt32: DsaOperation =
    DsaOperation("H16ToSI32", FloatConversion, floatConversionException) // Half16 -> SInt32
  val Half16ToSInt64: DsaOperation =
    DsaOperation("H16ToSI64", FloatConversion, floatConversionException) // Half16 -> SInt64
  val Float32ToSInt8: DsaOperation =
    DsaOperation("F32ToSI8", FloatConversion, floatConversionException) // Float32 -> SInt8
  val Float32ToSInt16: DsaOperation =
    DsaOperation("F32ToSI16", FloatConversion, floatConversionException) // Float32 -> SInt16
  val Float32ToSInt32: DsaOperation =
    DsaOperation("F32ToSI32", FloatConversion, floatConversionException) // Float32 -> SInt32
  val Float32ToSInt64: DsaOperation =
    DsaOperation("F32ToSI64", FloatConversion, floatConversionException) // Float32 -> SInt64
  val Double64ToSInt8: DsaOperation =
    DsaOperation("D64ToSI8", FloatConversion, floatConversionException) // Double64 -> SInt8
  val Double64ToSInt16: DsaOperation =
    DsaOperation("D64ToSI16", FloatConversion, floatConversionException) // Double64 -> SInt16
  val Double64ToSInt32: DsaOperation =
    DsaOperation("D64ToSI32", FloatConversion, floatConversionException) // Double64 -> SInt32
  val Double64ToSInt64: DsaOperation =
    DsaOperation("D64ToSI64", FloatConversion, floatConversionException) // Double64 -> SInt64

  // Unsigned Integer to Unsigned Integer (fill/discard zeros at MSB)
  val UInt8ToUInt16:  DsaOperation = DsaOperation("UI8ToUI16", FixedConversion, Set(FixedOverflow))
  val UInt8ToUInt32:  DsaOperation = DsaOperation("UI8ToUI32", FixedConversion, Set(FixedOverflow))
  val UInt8ToUInt64:  DsaOperation = DsaOperation("UI8ToUI64", FixedConversion, Set(FixedOverflow))
  val UInt16ToUInt8:  DsaOperation = DsaOperation("UI16ToUI8", FixedConversion, Set(FixedOverflow))
  val UInt16ToUInt32: DsaOperation = DsaOperation("UI16ToUI32", FixedConversion, Set(FixedOverflow))
  val UInt16ToUInt64: DsaOperation = DsaOperation("UI16ToUI64", FixedConversion, Set(FixedOverflow))
  val UInt32ToUInt8:  DsaOperation = DsaOperation("UI32ToUI8", FixedConversion, Set(FixedOverflow))
  val UInt32ToUInt16: DsaOperation = DsaOperation("UI32ToUI16", FixedConversion, Set(FixedOverflow))
  val UInt32ToUInt64: DsaOperation = DsaOperation("UI32ToUI64", FixedConversion, Set(FixedOverflow))
  val UInt64ToUInt8:  DsaOperation = DsaOperation("UI64ToUI8", FixedConversion, Set(FixedOverflow))
  val UInt64ToUInt16: DsaOperation = DsaOperation("UI64ToUI16", FixedConversion, Set(FixedOverflow))
  val UInt64ToUInt32: DsaOperation = DsaOperation("UI64ToUI32", FixedConversion, Set(FixedOverflow))

  // Unsigned Integer to Signed Integer (fill/extract zeros at MSB)
  val UInt8ToSInt8:   DsaOperation = DsaOperation("UI8ToSI8", FixedConversion, Set(FixedOverflow))
  val UInt8ToSInt16:  DsaOperation = DsaOperation("UI8ToSI16", FixedConversion, Set(FixedOverflow))
  val UInt8ToSInt32:  DsaOperation = DsaOperation("UI8ToSI32", FixedConversion, Set(FixedOverflow))
  val UInt8ToSInt64:  DsaOperation = DsaOperation("UI8ToSI64", FixedConversion, Set(FixedOverflow))
  val UInt16ToSInt8:  DsaOperation = DsaOperation("UI16ToSI8", FixedConversion, Set(FixedOverflow))
  val UInt16ToSInt16: DsaOperation = DsaOperation("UI16ToSI16", FixedConversion, Set(FixedOverflow))
  val UInt16ToSInt32: DsaOperation = DsaOperation("UI16ToSI32", FixedConversion, Set(FixedOverflow))
  val UInt16ToSInt64: DsaOperation = DsaOperation("UI16ToSI64", FixedConversion, Set(FixedOverflow))
  val UInt32ToSInt8:  DsaOperation = DsaOperation("UI32ToSI8", FixedConversion, Set(FixedOverflow))
  val UInt32ToSInt16: DsaOperation = DsaOperation("UI32ToSI16", FixedConversion, Set(FixedOverflow))
  val UInt32ToSInt32: DsaOperation = DsaOperation("UI32ToSI32", FixedConversion, Set(FixedOverflow))
  val UInt32ToSInt64: DsaOperation = DsaOperation("UI32ToSI64", FixedConversion, Set(FixedOverflow))
  val UInt64ToSInt8:  DsaOperation = DsaOperation("UI64ToSI8", FixedConversion, Set(FixedOverflow))
  val UInt64ToSInt16: DsaOperation = DsaOperation("UI64ToSI16", FixedConversion, Set(FixedOverflow))
  val UInt64ToSInt32: DsaOperation = DsaOperation("UI64ToSI32", FixedConversion, Set(FixedOverflow))
  val UInt64ToSInt64: DsaOperation = DsaOperation("UI64ToSI64", FixedConversion, Set(FixedOverflow))

  // Signed Integer to Unsigned Integer (fill/extract zeros at MSB)
  val SInt8ToUInt8:   DsaOperation = DsaOperation("SI8ToUI8", FixedConversion, Set(FixedOverflow))
  val SInt8ToUInt16:  DsaOperation = DsaOperation("SI8ToUI16", FixedConversion, Set(FixedOverflow))
  val SInt8ToUInt32:  DsaOperation = DsaOperation("SI8ToUI32", FixedConversion, Set(FixedOverflow))
  val SInt8ToUInt64:  DsaOperation = DsaOperation("SI8ToUI64", FixedConversion, Set(FixedOverflow))
  val SInt16ToUInt8:  DsaOperation = DsaOperation("SI16ToUI8", FixedConversion, Set(FixedOverflow))
  val SInt16ToUInt16: DsaOperation = DsaOperation("SI16ToUI16", FixedConversion, Set(FixedOverflow))
  val SInt16ToUInt32: DsaOperation = DsaOperation("SI16ToUI32", FixedConversion, Set(FixedOverflow))
  val SInt16ToUInt64: DsaOperation = DsaOperation("SI16ToUI64", FixedConversion, Set(FixedOverflow))
  val SInt32ToUInt8:  DsaOperation = DsaOperation("SI32ToUI8", FixedConversion, Set(FixedOverflow))
  val SInt32ToUInt16: DsaOperation = DsaOperation("SI32ToUI16", FixedConversion, Set(FixedOverflow))
  val SInt32ToUInt32: DsaOperation = DsaOperation("SI32ToUI32", FixedConversion, Set(FixedOverflow))
  val SInt32ToUInt64: DsaOperation = DsaOperation("SI32ToUI64", FixedConversion, Set(FixedOverflow))
  val SInt64ToUInt8:  DsaOperation = DsaOperation("SI64ToUI8", FixedConversion, Set(FixedOverflow))
  val SInt64ToUInt16: DsaOperation = DsaOperation("SI64ToUI16", FixedConversion, Set(FixedOverflow))
  val SInt64ToUInt32: DsaOperation = DsaOperation("SI64ToUI32", FixedConversion, Set(FixedOverflow))
  val SInt64ToUInt64: DsaOperation = DsaOperation("SI64ToUI64", FixedConversion, Set(FixedOverflow))

  // Signed Integer to Signer Integer
  val SInt8ToSInt16:  DsaOperation = DsaOperation("SI8ToSI16", FixedConversion, Set(FixedOverflow))
  val SInt8ToSInt32:  DsaOperation = DsaOperation("SI8ToSI32", FixedConversion, Set(FixedOverflow))
  val SInt8ToSInt64:  DsaOperation = DsaOperation("SI8ToSI64", FixedConversion, Set(FixedOverflow))
  val SInt16ToSInt8:  DsaOperation = DsaOperation("SI16ToSI8", FixedConversion, Set(FixedOverflow))
  val SInt16ToSInt32: DsaOperation = DsaOperation("SI16ToSI32", FixedConversion, Set(FixedOverflow))
  val SInt16ToSInt64: DsaOperation = DsaOperation("SI16ToSI64", FixedConversion, Set(FixedOverflow))
  val SInt32ToSInt8:  DsaOperation = DsaOperation("SI32ToSI8", FixedConversion, Set(FixedOverflow))
  val SInt32ToSInt16: DsaOperation = DsaOperation("SI32ToSI16", FixedConversion, Set(FixedOverflow))
  val SInt32ToSInt64: DsaOperation = DsaOperation("SI32ToSI64", FixedConversion, Set(FixedOverflow))
  val SInt64ToSInt8:  DsaOperation = DsaOperation("SI64ToSI8", FixedConversion, Set(FixedOverflow))
  val SInt64ToSInt16: DsaOperation = DsaOperation("SI64ToSI16", FixedConversion, Set(FixedOverflow))
  val SInt64ToSInt32: DsaOperation = DsaOperation("SI64ToSI32", FixedConversion, Set(FixedOverflow))

  // Trigonometric Function
  val FixedSine:   DsaOperation = DsaOperation("FixedSin", FixedArithmetics)
  val FixedCosine: DsaOperation = DsaOperation("FixedCos", FixedArithmetics)

  // Fixed Number Logical
  val LogicalNot: DsaOperation = DsaOperation("LNot", LogicalOperation)
  val LogicalAnd: DsaOperation = DsaOperation("LAnd", LogicalOperation)
  val LogicalOr:  DsaOperation = DsaOperation("LOr", LogicalOperation)
  val LogicalXor: DsaOperation = DsaOperation("LXor", LogicalOperation)

  // Bitwise Operation
  // Copy is mainly used for forcing a path passing through PE
  val Copy:       DsaOperation = DsaOperation("Copy", BitwiseOperation)
  val BitwiseNot: DsaOperation = DsaOperation("BNot", BitwiseOperation)
  val BitwiseOr:  DsaOperation = DsaOperation("BOr", BitwiseOperation)
  val BitwiseAnd: DsaOperation = DsaOperation("BAnd", BitwiseOperation)
  val BitwiseXor: DsaOperation = DsaOperation("BXor", BitwiseOperation)

  // Construct Properties for Data Type Conversion Operation, which are all 1-to-1
  val DataTypeConversionProp: Map[DsaOperation, (Int, Int)] = {
    val dataTypeConversion: Set[DsaOperation] = this.values
      .map(valueToDsaOperation)
      .filter {
        case op: DsaOperation => op.str.contains("To")
        case err => require(requirement = false, s"Variable $err should not belongs to Enumeration $this"); false
      }
      .map(valueToDsaOperation)
    // all data type conversion is 1 input and 1 output operation
    dataTypeConversion.map(op => op -> (1, 1)).toMap
  }

  /** Mapping from Operation -> (#Operand, #Result, minLatency(cycles), maxThroughput(cycles))
    * Latency, #cycle between operands' valid and results' valid, 0 means pure combinational logic operation
    * Throughput, #result can be produced per cycle, 1 means fully pipelined, Throughput == 1 / Instruction Interval)
    */
  val OperationNumOperandResult: Map[DsaOperation, (Int, Int)] = Map(
    // Integer Operation
    FixedAdd -> (2, 1),
    FixedHLAdd -> (1, 1),
    FixedSub -> (2, 1),
    FixedMul -> (2, 1),
    FixedPMulPAdd -> (3, 1),
    FixedPMulNAdd -> (3, 1),
    FixedNMulPAdd -> (3, 1),
    FixedNMulNAdd -> (3, 1),
    FixedDiv -> (2, 1),
    FixedMod -> (2, 1),
    Concat -> (2, 1),
    Select -> (3, 1),
    LeftShift -> (2, 1),
    RightShift -> (2, 1),
    Compare -> (2, 1),
    FixedMin -> (2, 1),
    FixedMax -> (2, 1),
    // Floating Point Operation
    FloatAdd -> (2, 1),
    FloatSub -> (2, 1),
    FloatMul -> (2, 1),
    FloatPMulPAdd -> (3, 1),
    FloatPMulNAdd -> (3, 1),
    FloatNMulPAdd -> (3, 1),
    FloatNMulNAdd -> (3, 1),
    FloatDiv -> (2, 1),
    FloatSqrt -> (1, 1),
    FloatCompare -> (2, 1),
    //metric Function
    FixedSine -> (1, 1),
    FixedCosine -> (1, 1),
    // Logical Operation
    LogicalNot -> (1, 1),
    LogicalAnd -> (2, 1),
    LogicalOr -> (2, 1),
    LogicalXor -> (2, 1),
    // Bitwise Operation
    Copy -> (1, 1),
    BitwiseNot -> (1, 1),
    BitwiseOr -> (2, 1),
    BitwiseAnd -> (2, 1),
    BitwiseXor -> (2, 1)
  ) ++ DataTypeConversionProp

  // Each operation defined should have its properties
  this.values.foreach {
    case op: DsaOperation =>
      require(OperationNumOperandResult.isDefinedAt(op), s"Please add properties (#operand, #result) for $op")
    case x => require(requirement = false, s"$x is not DSA supported operation")
  }
}
