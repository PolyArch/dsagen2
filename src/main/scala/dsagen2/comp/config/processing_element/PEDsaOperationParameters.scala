package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Parameters
import chisel3.util.log2Ceil
import dsagen2.comp.config.CompKeys.{MetaControl, RegFile}
import dsagen2.comp.config.ReconfParameters
import dsagen2.top.config.DSAFixedConfig.CTRL_MODE_BITS
import dsagen2.top.config.operation.DataType._
import dsagen2.top.config.operation.OperDataType._
import dsagen2.top.config.operation.OperDataTypeSet.setOpDataTypeEnhance
import dsagen2.top.config.operation.Operation.Copy
import dsagen2.top.config.operation.OperationGroup.BitwiseOperation
import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json._

import scala.collection.Set
import scala.collection.mutable.ListBuffer

/** Instruction Bundle derived from this case class
  *
  * @param givenOpDataTypeSet Given Operation + DataType Set
  */
case class PEDsaOperationParameters(
  givenOpDataTypeSet: Set[DsaOperDataType] = AllDsaOperationDataType,
  instSlotSize:       Int = 1,
  maxFifoDepth:       Int = 8, // 0 means dynamic fifo
  maxInstRepeatTime:  Int = 0,
  definedLatency:     Int = 0,
  isDynamic:          Boolean = true)
    extends ReconfParameters
    with JSONParsableParameters {

  // The Copy Operation is added by default and always, since it is Set
  // so we do not need to worry about the duplication
  def opDataTypeSet: Set[DsaOperDataType] = givenOpDataTypeSet ++ Set(Copy.toOpDataType)

  // Requirement
  require(instSlotSize > 0, s"At least one instruction need to be support in FU, but you have ")

  // Require given operation+dataType is defined
  opDataTypeSet.foreach { opDataType =>
    val fullSet: Set[DsaOperDataType] = AllDsaOperationDataType
    if (!fullSet.contains(opDataType)) {
      require(fullSet.contains(opDataType), s"$opDataType is not in full definition")
    }
  }

  // Sanity Check the OperationDataType
  private def sanityOpDataType(opDataType: String): Boolean = {
    opDataTypeSet.contains(opDataType)
  }

  // Get Data Type String
  private def getDataType(strOpDataType: String, doSanity: Boolean = true): Option[DsaDataType] = {
    if (doSanity) require(sanityOpDataType(strOpDataType), s"Given Operation and Data Type is illegal or not supported")
    strOpDataType.dataType
  }

  // Get DSA Operation String
  private def getDsaOperationStr(strOpDataType: String, doSanity: Boolean = true): String = {
    if (doSanity) require(sanityOpDataType(strOpDataType), s"Given Operation and Data Type is illegal or not supported")
    strOpDataType.operation.str
  }

  // Get the bit width of Data Type, 0 means bitwise
  def getDataTypeBits(strOpDataType: String): Option[Int] =
    string2opDataType(strOpDataType).totalBits

  // Get if it is bitwise
  def isBitwise(strOpDataType: String): Boolean = strOpDataType.is(BitwiseOperation)

  // Get the signed / unsigned
  def isUnsignedOperation(strOpDataType: String): Boolean = {
    require(sanityOpDataType(strOpDataType))
    strOpDataType.dataType match {
      case Some(t) => UnsignedNondecompGroup.contains(t)
      case None    => false // Bitwise operation
    }
  }

  // If it is float
  def isFloat(strOpDataType: String): Boolean = {
    require(sanityOpDataType(strOpDataType))
    strOpDataType.dataType match {
      case Some(t) => FloatNondecompGroup.contains(t)
      case None    => false // Bitwise operation
    }
  }

  // If it is fixed
  def isFixed(strOpDataType: String): Boolean = {
    require(sanityOpDataType(strOpDataType))
    strOpDataType.dataType match {
      case Some(t) => FixedNondecompGroup.contains(t)
      case None    => false // Bitwise operation
    }
  }

  // Get DsaOperationDataType String -> Encoding Mapping
  def getDsaOpDataTypeString2EncMap: Map[String, Int] =
    getDsaOpDataType2EncMap.map(kv => kv._1.toString -> kv._2)

  // Get DsaOperationDataType -> Encoding Mapping
  def getDsaOpDataType2EncMap: Map[DsaOperDataType, Int] = {
    // Initialize the start encoding number and buffer to hold the encoding
    val encodings:     ListBuffer[(DsaOperDataType, Int)] = ListBuffer[(DsaOperDataType, Int)]()
    var init_encoding: Int = 0
    // Specify the sequence of predefined operation
    // Copy is supported in all PE by default, it is used for pass through
    val predefODTs: Seq[DsaOperDataType] =
      Seq(bitwise2opDataType(Copy))
    // Allocate the encoding for those predefined operation
    for (idx <- predefODTs.indices) {
      val predefODT: DsaOperDataType = predefODTs(idx)
      if (opDataTypeSet.contains(predefODT)) {
        encodings += predefODT -> init_encoding
        init_encoding = init_encoding + 1
      }
    }
    // Calculate the rest of Operation + DataType set
    val restODTs: Set[DsaOperDataType] = opDataTypeSet -- predefODTs.toSet
    // Convert the set of operation + dataType to sequence and order it based on the String
    // This is the place that we make sure operation has fixed relative order
    val ordRestODTs: Seq[DsaOperDataType] = restODTs.toSeq.sortWith(_.toString < _.toString)
    // Assign the rest of operation in order
    ordRestODTs.zipWithIndex.foreach { case (opDataType, i) =>
      encodings += opDataType -> (i + init_encoding)
    }
    // Convert to map and return
    encodings.toMap
  }

  // Get Encoding -> DsaOperationDataType Mapping
  def getEnc2DsaOpDataTypeMap: Map[Int, String] = {
    getDsaOpDataTypeString2EncMap.map { case (str, i) => i -> str }
  }

  // Get Encoding -> isFloat Mapping
  def getEnc2FloatMap: Map[Int, Boolean] = {
    getEnc2DsaOpDataTypeMap.map { case (i, str) =>
      i -> isFloat(str)
    }
  }

  // Get Encoding for one DsaOperation_DataType
  def getEnc(opDataType: String): Int =
    getDsaOpDataTypeString2EncMap(opDataType)

  // Get DsaOperation_DataType from one encoding
  def getOperationDataType(encoding: Int): String = {
    getEnc2DsaOpDataTypeMap(encoding)
  }

  // Check whether support floating operation
  def supportFloat: Boolean = opDataTypeSet.map(_.toString).exists(isFloat)

  // Check whether support signed fixed
  def supportSignedFixed: Boolean =
    opDataTypeSet.exists(opDataType => isFixed(opDataType.toString) && !isUnsignedOperation(opDataType.toString))

  // check whether support unsigned fixed
  def supportUnsignedFixed: Boolean = opDataTypeSet.exists(opDataType => {
    isFixed(opDataType.toString) && isUnsignedOperation(opDataType.toString)
  })

  // check whether support fixed operation
  def supportFixed: Boolean = opDataTypeSet.map(_.toString).exists(isFixed)

  // check whether support bitwise
  def supportBitWise: Boolean = opDataTypeSet.map(_.toString).exists(isBitwise)

  // Get Finest Width
  def FinestWidth(fil: String => Boolean): Int =
    opDataTypeSet.map(_.toString).filter(fil).map(getDataTypeBits(_).getOrElse(Int.MaxValue)) min

  // Get Widest Width
  def WidestWidth(fil: String => Boolean): Int =
    opDataTypeSet.map(_.toString).filter(fil).map(getDataTypeBits(_).getOrElse(Int.MinValue)) max

  // Get the finest fixed operation bit width, 0 means no fixed operation
  def getFinestFixedWidth: Int = if (supportFixed) FinestWidth(isFixed) else 0

  // Get the widest fixed operation bit width, 0 means no fixed operation
  def getWidestFixedWidth: Int = if (supportFixed) WidestWidth(isFixed) else 0

  // Get the finest float operation bit width, 0 means no float operation
  def getFinestFloatWidth: Int = if (supportFloat) FinestWidth(isFloat) else 0

  // Get the widest float operation bit width, 0 means no float operation
  def getWidestFloatWidth: Int = if (supportFloat) WidestWidth(isFloat) else 0

  // Get the max number of operand
  def maxNumOperand: Int = opDataTypeSet.map(_.numOperand) max

  // Get the max number of result
  def maxNumResult: Int = opDataTypeSet.map(_.numResult) max

  // Calculate the number of opcode, one for each
  def numOpcode: Int = opDataTypeSet.size

  // CSR bits in total
  def csrBits(num_input: Int, num_output: Int)(implicit p: Parameters): Int =
    csrFieldBits(num_input, num_output).map(_._2).sum

  // Get Operation Opcode CSR Field Names and Bits
  def csrFieldBits(num_input: Int, num_output: Int)(implicit p: Parameters): Seq[(String, Int)] = {

    // Add register as source of operand and sink of result
    val numRegister: Int = p.lift(RegFile) match {
      case Some(regFileParam) => regFileParam.numReg
      case None               => 0
    }

    // The instruciton latency affected by register file
    val maxInstDelayCycle: Int = p.lift(RegFile) match {
      // if the register file is not async, then at least one cycle is needed for sync
      case Some(regFileParam) => if (!regFileParam.asyncRF) 1 else 0
      case None               => 0
    }

    // Check whether this instruction is controlled instruction
    val (inputCtrl, outputCtrl): (Boolean, Boolean) = p.lift(MetaControl) match {
      case Some(ctrlParam) => (ctrlParam.inputCtrl, ctrlParam.outputCtrl)
      case None            => (false, false)
    }

    // Calculate the number of bits for operands selection
    // Selection between input, register and nothing (means current does not need this operand position)
    val inputSelBits: Int = log2Ceil(num_input + numRegister + 1)

    // Calcualte selection bits for control input
    val inputCtrlSelBits: Int =
      if (num_input > 1 && inputCtrl) log2Ceil(num_input + 1) else 0

    // Calculate the delay selection bits for each operands
    val delaySelBits: Int =
      if (maxFifoDepth > 0) log2Ceil(maxFifoDepth + 1) else 0 // 0 max delay fifo depth is dynamic delay

    // Calculate the number of bits needed for result to each output port, value 0 means do not write to output port
    val resOutSelBits: Int = if (num_output > 0) log2Ceil(num_output + 1) else 0

    // Calculate the number of bits needed for direct the result to each resgietr, value 0 means to not write to reg
    val resRegSelBits: Int = if (numRegister > 0) log2Ceil(numRegister + 1) else 0

    // Calculate the number of bits needed for instruction delay
    val delayInstBits: Int = {
      // This is the operation natural latency
      val maxInstructionDelayFromOperation: Int = opDataTypeSet.maxLatency
      // Final latency
      val finalLatency: Int = maxInstructionDelayFromOperation.max(maxInstDelayCycle).max(definedLatency)
      if (finalLatency > 0) log2Ceil(finalLatency + 1) else 0
    }

    // Calcualte the fields for instruction repeat
    val repeatBits: Int = {
      if (instSlotSize > 1) {
        if (maxInstRepeatTime > 0) log2Ceil(maxInstRepeatTime + 1) else 0
      } else {
        0
      }
    }

    // This the encoding part for FUInstruction
    val instEncoding = for (instIdx <- 0 until instSlotSize) yield {
      val fieldsForEach: Seq[(String, Int)] = {

        // Instruciton is valid
        val instValid: Seq[(String, Int)] =
          Seq((s"Instruction_${instIdx}_Valid", 1))

        // Calculate the Operand selection from input and register
        val operSelFieldBits: Seq[(String, Int)] = {
          if (inputSelBits > 0) {
            (0 until maxNumOperand).map { operandIdx =>
              (s"Instruction_${instIdx}_OperandSel_$operandIdx", inputSelBits)
            }
          } else Nil
        }

        // Caluclate the fields encoding for ctrl mode
        val ctrlModeFieldBits: Seq[(String, Int)] = {
          if (inputCtrl || outputCtrl) {
            Seq((s"Instruction_${instIdx}_CtrlMode", CTRL_MODE_BITS))
          } else Nil
        }

        // Calculate the fields for control input selection
        val inputCtrlSelFieldBits: Seq[(String, Int)] = {
          if (inputCtrlSelBits > 0) Seq((s"Instruction_${instIdx}_CtrlInputSel", inputCtrlSelBits))
          else Nil
        }

        // Calculate the delay cycle for each operand
        val operandDelayFieldBits: Seq[(String, Int)] = {
          if (delaySelBits > 0 && !isDynamic) {
            (0 until maxNumOperand).map { operandIdx =>
              (s"Instruction_${instIdx}_OperandDelay_$operandIdx", delaySelBits)
            }
          } else Nil
        }

        // Calculate the Operation Field and CSRBits
        val opFieldBits: Seq[(String, Int)] =
          Seq((s"Instruction_${instIdx}_Opcode", log2Ceil(opDataTypeSet.size)))

        // Calculate the Demux for output Field and CSRBits
        val resOutFieldBits: Seq[(String, Int)] = {
          if (resOutSelBits > 0)
            (0 until maxNumResult).map { resultIdx =>
              (s"Instruction_${instIdx}_ResultOut_$resultIdx", resOutSelBits)
            }
          else Nil
        }

        // Calculate the Demux for register Field and CSRBits
        val resRegFieldBits: Seq[(String, Int)] = {
          if (resRegSelBits > 0)
            (0 until maxNumResult).map { resultIdx =>
              (s"Instruction_${instIdx}_ResultReg_$resultIdx", resRegSelBits)
            }
          else Nil
        }

        // Instruction delay field
        val instLatencyFieldBits: Seq[(String, Int)] = if (delayInstBits > 0) {
          Seq((s"Instruction_${instIdx}_Latency", delayInstBits))
        } else Nil

        // Instruction repeat field
        val instRepeatFieldBits: Seq[(String, Int)] = if (repeatBits > 0) {
          Seq((s"Instruction_${instIdx}_RepeatTime", repeatBits))
        } else Nil

        instValid ++
          operSelFieldBits ++
          ctrlModeFieldBits ++ inputCtrlSelFieldBits ++
          operandDelayFieldBits ++
          opFieldBits ++
          resOutFieldBits ++ resRegFieldBits ++
          instLatencyFieldBits ++ instRepeatFieldBits
      }
      fieldsForEach
    }
    instEncoding.reduce(_ ++ _)
  }

  /* ---------- JSON Emitter ---------- */

  import play.api.libs.json.Json

  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[PEDsaOperationParameters] {
    def writes(operationParam: PEDsaOperationParameters): JsObject = {
      val operations: Seq[String] = operationParam.getDsaOpDataTypeString2EncMap.toSeq.sortWith {
        case ((_, e1), (_, e2)) => e1 < e2
      }.map(_._1)
      json.deepMerge(
        Json.obj(
          fields = "OperationDataTypeSet" -> JsArray(operations.map(JsString)),
          "isDynamic" -> isDynamic,
          "instSlotSize" -> instSlotSize,
          "maxFifoDepth" -> maxFifoDepth,
          "maxInstRepeatTime" -> maxInstRepeatTime,
          "definedLatency" -> definedLatency
        )
      )
    }
  }
}

// Parameter Constructor
object PEDsaOperationParameters extends JSONParsableConstructor {
  def apply(json: JsValue): PEDsaOperationParameters = {
    PEDsaOperationParameters(
      givenOpDataTypeSet = (json \ "OperationDataTypeSet").as[Array[String]].map(string2opDataType).toSet,
      instSlotSize = (json \ "instSlotSize").as[Int],
      maxFifoDepth = (json \ "maxFifoDepth").as[Int],
      maxInstRepeatTime = (json \ "maxInstRepeatTime").as[Int],
      definedLatency = (json \ "definedLatency").as[Int],
      isDynamic = (json \ "isDynamic").as[Boolean]
    )
  }
}
