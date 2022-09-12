package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.exception.CompException.ComputationException
import dsagen2.comp.config.processing_element.PEDsaOperationParameters
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.OperDataType._

import scala.collection.Set
import scala.collection.mutable.ListBuffer

trait AluModuleImpl {
  implicit val p: Parameters
  val compNode:   CompNodeParameters
  val aluParam:   PEDsaOperationParameters
  val maxNumOperand: Int = aluParam.maxNumOperand
  val maxNumResult:  Int = aluParam.maxNumResult
  val opDataTypeSet: Set[DsaOperDataType] = aluParam.opDataTypeSet
  val exceptionSet:  Set[ComputationException] = opDataTypeSet.map(_.operation.getExceptions).reduce(_ ++ _)

  /* Virtual IO */
  val aluOperands: Vec[DecoupledIO[UInt]]
  val aluOpcode:   Option[UInt]
  val aluResults:  Vec[DecoupledIO[UInt]]
  // TODO: we don't have a plan to deal with exception in ALU for now
  val aluExceptions: EnumBundle

  // Get the data path bitwidth
  def dataPathBits: Int = compNode.compBits

  /** Given A set of Operation+DataType, return a group of Operation+DataType and the rest set of Operation+DataType
    *
    * @param opDataTypeSet Given Operation+DatType set
    * @return (first group of decomposable operation+datatype, rest of operation+datatype)
    */
  private def operationGrouping(opDataTypeSet: Set[DsaOperDataType]): (Set[DsaOperDataType], Set[DsaOperDataType]) = {
    // Find a group of operation that share common implementations
    // TODO: how to do this grouping is critical
    val headImpl:          IsFunctionUnitImplementation = opDataTypeSet.head.operation.getImpl
    val sameImplOperation: Set[DsaOperDataType] = opDataTypeSet.filter(_.operation.getImpl == headImpl)
    val rest:              Set[DsaOperDataType] = opDataTypeSet -- sameImplOperation
    (sameImplOperation, rest)
  }

  // Convert the Operation+DataType set to actual Function Unit with preliminary composable FU support
  private def toFunctionUnits(opDataTypeSet: Set[DsaOperDataType]): List[(Set[DsaOperDataType], FunctionUnitModule)] = {
    var restOperDataType: Set[DsaOperDataType] = opDataTypeSet
    val fus:              ListBuffer[(Set[DsaOperDataType], FunctionUnitModule)] = ListBuffer()
    while (restOperDataType.nonEmpty) {
      val (selectGroup, rest) = operationGrouping(restOperDataType)
      val fu = Module(
        new FunctionUnitModule(
          opDataTypeSet = selectGroup,
          opcodeMapping = aluParam.getDsaOpDataType2EncMap,
          dataPathBits = dataPathBits,
          numTotalOperation = opDataTypeSet.size
        )(p)
      )
      restOperDataType = rest
      val pair = (selectGroup, fu)
      fus += pair
    }
    fus.result()
  }

  // Sanity Check
  def sanity(): Unit = {
    aluResults.foreach { res =>
      require(
        res.bits.getWidth == dataPathBits,
        s"Result Bits ${res.getWidth} is not equal to dataPathBits $dataPathBits"
      )
    }
    aluOperands.foreach { op =>
      require(
        op.bits.getWidth == dataPathBits,
        s"Operand Bits ${op.getWidth} is not equal to dataPathBits $dataPathBits"
      )
    }
  }

  // Connect the compute wire
  def compute(): Unit = {
    sanity() // Sanity Check
    // Combinational Connection
    val opDTs2fus: List[(Set[DsaOperDataType], FunctionUnitModule)] = toFunctionUnits(opDataTypeSet)
    val fus:       List[FunctionUnitModule] = opDTs2fus.map(_._2)
    val fuSelected: Seq[Bool] = opDTs2fus.map { case (opDTset, _) =>
      val encGroup:    Set[Int] = opDTset.map(aluParam.getDsaOpDataType2EncMap(_))
      val matchOpcode: Bool = VecInit(encGroup.toSeq.map(e => e.U === aluOpcode.getOrElse(0.U))).asUInt().orR()
      matchOpcode
    }
    // ALU Operands -> Function Unit (Valid, Bits)
    fus.zip(fuSelected).foreach { case (fu, selected) =>
      fu.Operands.zip(aluOperands).foreach { case (fuOp, aluOp) =>
        require(fuOp.bits.getWidth == aluOp.bits.getWidth, s"Assign bit width mismatch")
        fuOp.bits := Mux(selected, aluOp.bits, 0.U)
        fuOp.valid := Mux(selected, aluOp.valid, false.B)
      }
    }
    // ALU Operands <- Function Unit (Ready, Or gate connect)
    aluOperands.zipWithIndex.foreach { case (aluOper, operIdx) =>
      aluOper.ready := VecInit(
        fus.zip(fuSelected).map { case (fu, selected) =>
          Mux(
            selected, {
              // FU only has the number of operand that it needs
              if (operIdx < fu.Operands.length) fu.Operands(operIdx).ready
              else false.B
            },
            false.B
          )
        }
      ).asUInt().orR()
    }
    // ALU Opcode -> Function Unit's Opcode
    fus.foreach { fu =>
      (fu.Opcode, aluOpcode) match {
        case (Some(fuOp), Some(aluOp)) =>
          require(fuOp.getWidth == aluOp.getWidth, s"Assign bit width mismatch")
          fuOp := aluOp
        case _ =>
      }
    }
    // Function Unit's exception -> ALU's exception
    EnumBundle.connect(aluExceptions, fus.map(_.Exception).reduce(EnumBundle.merge))

    // Function Unit's Results -> ALU Results (Bitwise Or gate merge)
    aluResults.zipWithIndex.foreach { case (result, idx) =>
      result.bits := Mux1H(fuSelected, fus.map(_.Results(idx).bits))
      result.valid := Mux1H(fuSelected, fus.map(_.Results(idx).valid))
    }

    // Function Unit's Ready <- ALU Ready
    fus.foreach { fu =>
      // This is fine, because this will only zip the common part
      fu.Results.zip(aluResults).foreach { case (fuRs, aluRs) =>
        fuRs.ready := aluRs.ready
      }
    }
  }
}
