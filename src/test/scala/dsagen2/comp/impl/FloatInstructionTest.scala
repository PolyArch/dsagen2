package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import dsagen2.top.config.operation.DataType._
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation._
import dsagen2.util.AppUtil.emitFuVerilog
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.Set

class FloatInstructionTest extends AnyFlatSpec{
  implicit val p : Parameters = Parameters.empty
  "Function Unit of Floating Point Instruction Verilog Emitter" should "works for Float Add and Sub" in {
    val isa : Set[DsaOperDataType] = Set(
      //FloatAdd + Mini8x8, // 8-bit floating number may not work in berkeley-hardfloat
      FloatAdd + Half16x2,
      FloatAdd + Double64,
      FloatSub + Float32x2,
      FloatSub + Half16x4
    )
    emitFuVerilog(isa, 64)
  }
  it should "works for Float Compare" in {
    val isa : Set[DsaOperDataType] = FloatCompare.withAllDataType
    emitFuVerilog(isa, 512)
  }
  it should "works for Float Division and Square Root" in {
    val isa : Set[DsaOperDataType] = Set(
      FloatDiv + Half16x2,
      FloatSqrt + Double64,
      FloatSqrt + Float32x2,
      FloatDiv + Float32,
      FloatDiv + Half16x4
    )
    emitFuVerilog(isa, 64)
  }
  it should "works for Float Multiplication" in {
    val isa : Set[DsaOperDataType] = Set(
      FloatMul + Half16,
      FloatMul + Half16x2,
      FloatMul + Half16x4,
      FloatMul + Float32x2,
      FloatMul + Float32,
      FloatMul + Double64,
    )
    emitFuVerilog(isa, 64)
  }
  it should "works for Float MADD" in {
    val isa : Set[DsaOperDataType] = Set(
      FloatPMulPAdd + Half16x2,
      FloatPMulPAdd + Half16x4,
      FloatPMulPAdd + Double64,
      FloatPMulNAdd + Half16,
      FloatPMulNAdd + Half16x2,
      FloatPMulNAdd + Float32x2,
      FloatPMulNAdd + Float32,
      FloatNMulPAdd + Half16,
      FloatNMulPAdd + Half16x4,
      FloatNMulPAdd + Float32,
      FloatNMulPAdd + Double64,
      FloatNMulNAdd + Half16,
      FloatNMulNAdd + Half16x2,
      FloatNMulNAdd + Float32,
      FloatNMulNAdd + Double64,
    )
    emitFuVerilog(isa, 64)
  }
  it should "works for Float to Float conversion" in {
    val isa : Set[DsaOperDataType] = Half16ToFloat32.withAllDataType ++ Double64ToFloat32.withAllDataType
    emitFuVerilog(isa, 512)
  }
  it should "works for Float to Integer conversion" in {
    val isa : Set[DsaOperDataType] = (Half16ToUInt16.withAllDataType ++ Half16ToSInt16.withAllDataType).take(10)
    emitFuVerilog(isa, 512)
  }
  it should "works for Integer to Float Conversion" in {
    val isa : Set[DsaOperDataType] = (UInt16ToHalf16.withAllDataType ++ SInt16ToHalf16.withAllDataType).take(10)
    emitFuVerilog(isa, 512)
  }
}
