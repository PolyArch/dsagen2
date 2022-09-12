package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import dsagen2.top.config.operation.DataType._
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation._
import dsagen2.util.AppUtil._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.Set

class FixedInstructionTest extends AnyFlatSpec{
  implicit val p : Parameters = Parameters.empty

  "Function Unit of Fixed Point Instruction Verilog Emitter " should "works for Fixed Add and Sub" in {
    val isa : Set[DsaOperDataType] =
      Set(
        FixedSub + UnsignedInt64,
        FixedAdd + UnsignedInt32x2,
        FixedSub + UnsignedInt32,
        FixedAdd + UnsignedInt16x4,
        FixedAdd + UnsignedInt8x4,
        FixedSub + UnsignedInt8x4,
        FixedSub + UnsignedInt8x2,
      )
    emitFuVerilog(isa, 128)
  }
  it should "works for Fixed High Low Add" in {
    val isa : Set[DsaOperDataType] = Set(
      FixedHLAdd + SignedInt32x2,
      FixedHLAdd + SignedInt32,
      FixedHLAdd + SignedInt64
    )
    emitFuVerilog(isa, 64)
  }
  it should "works for Fixed Compare" in {
    val isa : Set[DsaOperDataType] = Compare.withAllDataType
    emitFuVerilog(isa, 512)
  }
  it should "works for Fixed Concat" in {
    val isa : Set[DsaOperDataType] = Set(Concat + UnsignedInt64, Concat + SignedInt32, Concat + UnsignedInt16x4)
    emitFuVerilog(isa, 64)
  }
  it should "works for Fixed Division" in {
    val isa : Set[DsaOperDataType] = Set(FixedDiv + UnsignedInt64, FixedDiv + UnsignedInt16x4, FixedDiv + SignedInt8x4)
    emitFuVerilog(isa, 64)
  }
  it should "works for Fixed Modulo" in {
    val isa : Set[DsaOperDataType] = Set(FixedMod + UnsignedInt64, FixedMod + UnsignedInt16x4, FixedMod + SignedInt8x4)
    emitFuVerilog(isa, 64)
  }
  it should "works for Fixed Left Shift" in {
    val isa : Set[DsaOperDataType] = Set(LeftShift + UnsignedInt8x4, LeftShift + UnsignedInt8x8, LeftShift + SignedInt64)
    emitFuVerilog(isa, 64)
  }
  it should "works for Fixed Multiplication" in {
    val isa : Set[DsaOperDataType] = Set(FixedMul + UnsignedInt8x8, FixedMul + SignedInt64, FixedMul + UnsignedInt16x2)
    emitFuVerilog(isa, 64)
  }
  it should "works for Fixed MADD (multiplication + addition)" in {
    val isa : Set[DsaOperDataType] = Set(
      FixedPMulPAdd + UnsignedInt8x8,
      FixedPMulNAdd + SignedInt64x2,
      FixedNMulPAdd + UnsignedInt32x2,
      FixedNMulPAdd + UnsignedInt32x4
    )
    emitFuVerilog(isa, 128)
  }
  it should "works for Fixed Right Shift" in {
    val isa : Set[DsaOperDataType] = Set(RightShift + UnsignedInt8x4, RightShift + UnsignedInt8x8, RightShift + SignedInt64)
    emitFuVerilog(isa, 64)
  }
  it should "works for Fixed Selection" in {
    val isa : Set[DsaOperDataType] = Set(Select + UnsignedInt32x2, Select + SignedInt8x4, Select + UnsignedInt64)
    emitFuVerilog(isa, 64)
  }
  it should "works for Min" in {
    val isa : Set[DsaOperDataType] = Set(FixedMin + UnsignedInt64x4, FixedMin + SignedInt64, FixedMin + SignedInt8x2)
    emitFuVerilog(isa, 256)
  }
  it should "works for Max" in {
    val isa : Set[DsaOperDataType] = Set(FixedMax + UnsignedInt64x4, FixedMax + SignedInt64, FixedMax + SignedInt8x2)
    emitFuVerilog(isa, 256)
  }
}
