package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.{DsaDataType, FixedGroup}
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet.setOpDataTypeEnhance
import dsagen2.top.config.operation.Operation.{DsaOperation, FixedAdd, FixedSub}

import scala.collection.Set

// A + B
object AddSub extends IsFunctionUnitImplementation {
  val supportOperation: Set[DsaOperation] = Set(FixedAdd, FixedSub)
  val supportDataType:  Set[DsaDataType] = FixedGroup
  val latency:          Int = 0
  val throughput:       Int = 1

  /** Hardware Function Template for Data Unit, will be overwrite by actual implementation
    *
    * @param inputBits     Input Bits from each operand
    * @param inputValids   Input valid bit from each operand
    * @param outputReadys  Downward Readies from each results
    * @param opcode        Opcode Wire
    * @param opDataTypeSet Operation+DataType Set for this function unit
    * @param encoding      mapping from Operation+DataType to opcode number
    * @param p             Hidden Processing Element parameters
    * @return (outputBits, outputValids, inputReadys, exception)
    */
  def implement(
    inputBits:     Seq[UInt],
    inputValids:   Seq[Bool],
    outputReadys:  Seq[Bool],
    opcode:        UInt,
    opDataTypeSet: collection.Set[DsaOperDataType],
    encoding:      Map[DsaOperDataType, Int],
    clock:         Clock,
    reset:         Reset
  )(
    implicit p: Parameters
  ): (Seq[UInt], Seq[Bool], Seq[Bool], EnumBundle) = {

    // Extract data path bits
    val dataPathBits: Int = inputBits.head.getWidth
    // A is original
    val A: UInt = inputBits.head
    // positive B
    val posB: UInt = inputBits(1)
    // bit inverted B
    val invB: Option[UInt] = if (opDataTypeSet.operations.contains(FixedSub)) Some((~posB).asUInt()) else None
    // almostB is 1 less than negative B only when Add and Sub are supported at the same time
    // almostB will be invB if only FixedSub exist, and will be positive B if only FixedAdd exists
    val almostB: UInt = {
      if (invB.isDefined && opDataTypeSet.has(FixedAdd)) {
        // Switch to inv B when opcode is FixedSub
        val lut: Seq[(UInt, UInt)] = opDataTypeSet.map { opt =>
          if (opt.operation == FixedSub) encoding(opt).U -> invB.get
          else encoding(opt).U -> posB
        }.toSeq
        MuxLookup(opcode, posB, lut)
      } else if (opDataTypeSet.has(FixedSub)) {
        invB.get
      } else {
        posB
      }
    }

    // Create output Valid and input Ready
    val outputValid: Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:  Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)

    // Find all granularity
    val unitBitsSet: Set[Int] = opDataTypeSet.unitBitsSet

    // Find the widest compBits for each granularity
    val unit2WidestData: Map[Int, Int] = unitBitsSet.map { unitBits =>
      // find all data type with this unit Bits
      val compBitsSet: Set[Int] = opDataTypeSet.filter(_.dataType.get.unitBits == unitBits).map(_.dataType.get.compBits)
      // Get the widest compBits
      unitBits -> compBitsSet.max
    }.toMap

    // Form widest subnet for each granularity, TODO: use finer granularity to support coarser granularity
    import dsagen2.util.UIntUtil.groupBitsAs
    val unit2vecResult: Map[Int, Seq[UInt]] = unitBitsSet.map { unitBits =>
      val wideDataBits: Int = unit2WidestData(unitBits)
      // only take the needed part for this granularity
      val vecA: Seq[UInt] = groupBitsAs(A(wideDataBits - 1, 0), unitBits)
      val vecB: Seq[UInt] = groupBitsAs(almostB(wideDataBits - 1, 0), unitBits)
      // return
      unitBits -> vecA.zip(vecB).map(ab => ab._1 + ab._2)
    }.toMap

    // Form opcode to outputBitsLut for further use of outputWire
    val outputBitsLut: Seq[(UInt, UInt)] = opDataTypeSet.toSeq.map { opt =>
      val enc:      Int = encoding(opt)
      val op:       DsaOperation = opt.operation
      val dt:       DsaDataType = opt.dataType.get
      val unitBits: Int = dt.unitBits
      val vecW:     Int = dt.vecWidth
      val almostVC: Seq[UInt] = unit2vecResult(unitBits)
      val vC: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
        // Almost C is sum for FixedAdd and 1 less than actual result for FixedSub
        val actualC: UInt = if (vecIdx < vecW) op match {
          // Calculation falls in operation definition
          case FixedAdd => almostVC(vecIdx)
          case FixedSub => almostVC(vecIdx) + 1.U(1.W)
          case _        => require(requirement = false, s"FixedAddSub only support $supportOperation"); almostVC(vecIdx)
        }
        else {
          // Calculation falls out of the operation definition
          0.U(unitBits.W)
        }
        require(
          actualC.getWidth == unitBits,
          s"Width (${actualC.getWidth}) for each unit " +
            s"is different with the operation definition $unitBits"
        )
        actualC
      }
      enc.U -> VecInit(vC).asUInt()
    }
    val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val outputBits = Mux(outputValid, outputBitsWire, 0.U)

    // No exception for Add and Sub
    val exception: EnumBundle = Wire(new EnumBundle(Set.empty))

    (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
  }
}
