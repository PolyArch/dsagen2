package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation.{FixedCosine, FixedSine}
import dsagen2.util.AppUtil.emitFuVerilog
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.Set

class TrigonometricInstructionTest extends AnyFlatSpec{
  implicit val p : Parameters = Parameters.empty
  "Trigonometric Function" should "works for Sine Function" in {
    val isa : Set[DsaOperDataType] = FixedSine.withAllDataType.take(5)
    emitFuVerilog(isa, 512)
  }
 it should "works for Cosine Function" in {
    val isa : Set[DsaOperDataType] = FixedCosine.withAllDataType.take(5)
    emitFuVerilog(isa, 512)
  }
}
