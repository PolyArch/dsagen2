package dsagen2.top.config.operation

import dsagen2.top.config.operation.DataType.{AllGroup, FixedGroup, FloatGroup}
import dsagen2.top.config.operation.Operation.DsaOperation

/** This is the group of operation, each group is related to a set of data type set
  */
object OperationGroup extends Enumeration {
  protected[dsagen2] case class DsaOperGroup(str: String, private val supportDataType: Set[DataType.DsaDataType])
      extends super.Val {

    // Not all data Type in supportDataType is applicable, replace supportDataType with applicableDataTypes
    def applicableDTs(op: DsaOperation): Set[DataType.DsaDataType] = {
      op.opGroup match {
        case FixedArithmetics => supportDataType
        case FloatArithmetics => supportDataType
        case FixedConversion =>
          val (sourceDT, sinkDT, isDTconvert) = op.sourceSinkDT
          require(isDTconvert, s"Parsed result from Operation String is saying this operation $op is not conversion")
          require(
            sourceDT.isFixed && sinkDT.isFixed,
            s"Fixed Data Type conversion requires both are fixed data type, " +
              s"but source = $sourceDT, sink = $sinkDT"
          )
          supportDataType.filter(dt => dt.isFixed && dt.unitBits >= sourceDT.unitBits && dt.unitBits >= sinkDT.unitBits)
        case FloatConversion =>
          val (sourceDT, sinkDT, isDTconvert) = op.sourceSinkDT
          require(isDTconvert, s"Parsed result from Operation String is saying this operation $op is not conversion")
          require(
            sourceDT.isFloat || sinkDT.isFloat,
            s"Floating Data Type conversion should have at least on is floating"
          )
          supportDataType.filter(dt => dt.isFloat && dt.unitBits >= sourceDT.unitBits && dt.unitBits >= sinkDT.unitBits)
        case LogicalOperation => supportDataType
        case BitwiseOperation => require(supportDataType.isEmpty, s"non empty data Type set?"); Set.empty
        case err              => require(requirement = false, s"Unknown operation group $err"); Set.empty
      }
    }
  }

  // Fixed Operation for Fixed Data Type
  val FixedArithmetics: DsaOperGroup = DsaOperGroup("Fixed", FixedGroup)

  // Float Operation for Floating Point
  val FloatArithmetics: DsaOperGroup = DsaOperGroup("Float", FloatGroup)

  // Data Type Conversion related only to integer
  val FixedConversion: DsaOperGroup = DsaOperGroup("I2I", FixedGroup)

  // Data Type Conversion related to float
  val FloatConversion: DsaOperGroup = DsaOperGroup("F2F", FloatGroup)

  // Logical for all
  val LogicalOperation: DsaOperGroup = DsaOperGroup("Logical", AllGroup)

  // Bitwise has nothing to do with data type
  val BitwiseOperation: DsaOperGroup = DsaOperGroup("Bitwise", Set())
}
