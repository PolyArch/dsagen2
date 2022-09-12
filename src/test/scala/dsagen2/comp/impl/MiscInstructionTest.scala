package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import dsagen2.top.config.operation.DataType.{SignedInt16x4, UnsignedInt16x2}
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation
import dsagen2.top.config.operation.Operation._
import dsagen2.util.AppUtil.emitFuVerilog
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.Set

class MiscInstructionTest extends AnyFlatSpec{
  implicit val p : Parameters = Parameters.empty
  "Bitwise operation" should "works for bitwise And" in {
    val isa : Set[DsaOperDataType] = Set(Operation.BitwiseAnd.toOpDataType)
    emitFuVerilog(isa, 128)
  }
  it should "works for bitwise Not" in {
    val isa : Set[DsaOperDataType] = Set(Operation.BitwiseNot.toOpDataType)
    emitFuVerilog(isa, 64)
  }
  it should "works for bitwise Or" in {
    val isa : Set[DsaOperDataType] = Set(Operation.BitwiseOr.toOpDataType)
    emitFuVerilog(isa, 128)
  }
  it should "works for bitwise Xor" in {
    val isa: Set[DsaOperDataType] = Set(Operation.BitwiseXor.toOpDataType)
    emitFuVerilog(isa, 32)
  }
  it should "works for Copy" in {
    val isa : Set[DsaOperDataType] = Set(Operation.Copy.toOpDataType)
    emitFuVerilog(isa, 32)
  }
  it should "works for Fixed to Fixed Conversion" in {
    val isa : Set[DsaOperDataType] =
      UInt8ToSInt8.withAllDataType.take(2) ++ UInt8ToSInt32.withAllDataType.take(2) ++ UInt32ToSInt16.withAllDataType.take(2) ++
        UInt8ToUInt32.withAllDataType.take(2) ++ UInt32ToUInt16.withAllDataType.take(2) ++
        SInt8ToSInt32.withAllDataType.take(2) ++ SInt32ToSInt16.withAllDataType.take(2) ++
        SInt16ToUInt16.withAllDataType.take(2) ++ SInt8ToUInt32.withAllDataType.take(2) ++ SInt32ToUInt16.withAllDataType.take(2)
    emitFuVerilog(isa, 512)
  }
  it should "works for Logical Operation" in {
    val isa : Set[DsaOperDataType] = Set(LogicalNot + UnsignedInt16x2, LogicalAnd + SignedInt16x4)
    emitFuVerilog(isa, 64)
  }
}
