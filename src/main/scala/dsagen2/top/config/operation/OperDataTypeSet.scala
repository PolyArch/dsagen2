package dsagen2.top.config.operation

import dsagen2.top.config.operation.DataType.DsaDataType
import dsagen2.top.config.operation.OperDataType.{AllDsaOperDataTypeID, DsaOperDataType}
import dsagen2.top.config.operation.Operation.DsaOperation

import scala.collection.Set
import scala.language.implicitConversions

object OperDataTypeSet {
  class SetEnhancedOperationDataType(opDataTypeSet: Set[DsaOperDataType]) {
    // Get the max #operand and #result
    def maxNumOperand: Int = opDataTypeSet.map(_.operation.numOperand).max

    def maxLatency: Int = opDataTypeSet.map(_.getImpl.latency).max

    def maxNumResult: Int = opDataTypeSet.map(_.operation.numResult).max

    // Get the max/min for all data Type's totalBits and unitBits and vecWidth
    def maxDataBits: Int = opDataTypeSet
      .map(_.dataType match {
        case Some(dT) => dT.compBits;
        case None     => Int.MinValue
      })
      .max

    def minDataBits: Int = opDataTypeSet
      .map(_.dataType match {
        case Some(dT) => dT.compBits;
        case None     => Int.MaxValue
      })
      .min

    def maxUnitBits: Int = opDataTypeSet
      .map(_.dataType match {
        case Some(dT) => dT.unitBits;
        case None     => Int.MinValue
      })
      .max

    def minUnitBits: Int = opDataTypeSet
      .map(_.dataType match {
        case Some(dT) => dT.unitBits;
        case None     => Int.MaxValue
      })
      .min

    def maxVecWidth: Int = opDataTypeSet
      .map(_.dataType match {
        case Some(dT) => dT.vecWidth;
        case None     => Int.MinValue
      })
      .max

    def minVecWidth: Int = opDataTypeSet
      .map(_.dataType match {
        case Some(dT) => dT.vecWidth;
        case None     => Int.MaxValue
      })
      .min

    // gather the unitBits for all Operation+DataType
    def unitBitsSet: Set[Int] = opDataTypeSet.map(_.unitBits.getOrElse(-1)) - (-1)

    // Return operations/dataTypes
    def operations: Set[DsaOperation] = opDataTypeSet.map(_.operation)

    def dataTypes: Set[DsaDataType] = opDataTypeSet.filter(_.dataType.isDefined).map(_.dataType.get)

    // Contain a certain operation
    def has(op: DsaOperation): Boolean = this.operations.contains(op)

    // A compact name that combine all operations
    def compactName: String = {
      val nameAsFunction: String = opDataTypeSet.map(_.compactString).reduce(_ + "_" + _)
      val nameAsID:       String = opDataTypeSet.map(AllDsaOperDataTypeID(_).toString).reduce(_ + "_" + _)
      if (nameAsFunction.length > 150) nameAsID
      else nameAsFunction
    }

    // The max opcode number of this set
    def maxOpcode(mapping: Map[DsaOperDataType, Int]): Int = opDataTypeSet.map(mapping(_)).max
    // Find one implementation that support this set
  }

  implicit def setOpDataTypeEnhance(set: Set[DsaOperDataType]): SetEnhancedOperationDataType =
    new SetEnhancedOperationDataType(set)
}
