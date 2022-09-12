package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import dsagen2.top.config.operation.OperDataType.{AllDsaOperDataTypeID, AllDsaOperationDataType, DsaOperDataType}
import dsagen2.util.AppUtil.{emitFuVerilog, writeOutputFile}
import org.scalatest.flatspec.AnyFlatSpec

class FuGenTest extends AnyFlatSpec{
  "Operation+DataType to Code, Number of Operand, Number of Result" should "dump csv file" in {
    /* Collect the instruction and dump the code name file*/
    val opDataType2codeStr : String = AllDsaOperDataTypeID.map{
      case (DsaOperDataType(operation, dataType, _), idx) =>
        val str : String = dataType match {
          case Some(dt) =>  operation.toString + s", ${dt.toString}, " + idx.toString + ", "
          case None =>      operation.toString + s", bitwise       , " + idx.toString + ", "
        }
        str + s"${operation.numOperand}, ${operation.numResult}" + "\n"
    }.reduce(_ + _)
    val header : String = "Operation, DataType, Code, #Operand, #Result\n"
    val fullString : String = header + opDataType2codeStr
    writeOutputFile("vsrc", "OpDataTypeCode.csv", fullString)
  }

  val tobeGen: collection.Set[DsaOperDataType] = sys.env.get("ALL") match {
    case Some(_) =>  AllDsaOperationDataType
    case None => AllDsaOperationDataType.take(10)
  }
  s"${tobeGen.size} Operations and its DataType" should "be able to generate verilog" in {
    implicit val p : Parameters = Parameters.empty
    tobeGen.par.foreach{opt =>
      opt.totalBits match {
        case Some(bw) => emitFuVerilog(Set(opt), bw)
        case None => emitFuVerilog(Set(opt), 64)
      }
    }
  }
}
