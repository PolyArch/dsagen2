package dsagen2.comp.config.processing_element

import dsagen2.top.config.operation
import dsagen2.top.config.operation.DataType.{DsaDataType, SignedInt32}
import dsagen2.top.config.operation.OperDataType._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.config.operation.OperationGroup.{BitwiseOperation, FixedArithmetics}
import org.scalatest.flatspec.AnyFlatSpec

class PEDsaOperationTest extends AnyFlatSpec {
  val para : PEDsaOperationParameters = PEDsaOperationParameters()


  "DSA Operation Encoding" should "Enc2OpDataType and OpDataType2Enc has same size" in {
    require(para.getDsaOpDataTypeString2EncMap.size == para.getEnc2DsaOpDataTypeMap.size,
      s"Enc2OpDataType (size = ${para.getDsaOpDataTypeString2EncMap.size}) and " +
        s"OpDataType2Enc (size = ${para.getEnc2DsaOpDataTypeMap.size}) has different size, " +
        s"diff String = ${para.getDsaOpDataTypeString2EncMap.keys.toSet -- para.getEnc2DsaOpDataTypeMap.values.toSet}")
  }
  it should "print the OpDataType 2 Encoding" in {
    para.getDsaOpDataTypeString2EncMap
      //.foreach(println)
  }

  /**
   * Test for 32-bit operation
   */
  val opSet : Set[DsaOperation] =
    operation.Operation.values.filter(_.is(FixedArithmetics)).map(valueToDsaOperation)
  val typeSet : Set[DsaDataType] = Set(SignedInt32)
  val para32 : PEDsaOperationParameters =
    PEDsaOperationParameters(crossproductOpDataType(opSet, typeSet))
  "Fixed Signed 32-bit operation" should "Finest width and widest width are both 32" in {
    require(para32.getFinestFixedWidth == para32.getWidestFixedWidth)
    require(para32.getFinestFixedWidth == 32)
  }
  it should "only support 32-bit signed fixed operation" in {
    require(!para32.supportUnsignedFixed, s"Parameter = $para32")
    require(para32.supportSignedFixed, s"Parameter = $para32")
    require(!para32.supportBitWise ||
      (para32.opDataTypeSet.map(_.toString).filter(para32.isBitwise) == Set(Copy.toString)),
      s"Parameter should not contains bitwise operation except Copy, " +
      s"but it has : ${para32.opDataTypeSet.map(_.toString).filter(para32.isBitwise)}")
    require(!para32.supportFloat, s"Parameter = $para32")
  }

  /**
   * Test for bitwise operation
   */
  val bitwiseSet : Set[DsaOperation] =
    operation.Operation.values.filter(_.is(BitwiseOperation)).map(valueToDsaOperation)
  val bitwiseOpSet : Set[DsaOperDataType] = bitwiseSet.map(_.toOpDataType)
  val paraBitwise : PEDsaOperationParameters = PEDsaOperationParameters(bitwiseOpSet)
  "Bitwise operation" should "only support bitwise operation" in {
    require(!paraBitwise.supportFloat)
    require(!paraBitwise.supportFixed)
    require(!paraBitwise.supportUnsignedFixed)
    require(!paraBitwise.supportSignedFixed)
    require(paraBitwise.supportBitWise)
  }
}
