package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util._
import dsagen2.comp.config.exception.CompException.InvalidOperation
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.comp.impl.ip.FComp_D64
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.enumeration.EnumEncodeMethod
import dsagen2.top.config.operation.DataType.FloatGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.Operation.FloatCompare
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import hardfloat.{CompareRecFN, recFNFromFN}

import scala.collection.Set
import scala.collection.mutable.ListBuffer

object Compare extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(FloatCompare)
  val supportDataType:  collection.Set[DataType.DsaDataType] = FloatGroup
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
    opDataTypeSet: collection.Set[OperDataType.DsaOperDataType],
    encoding:      Map[OperDataType.DsaOperDataType, Int],
    clock:         Clock,
    reset:         Reset
  )(
    implicit p: config.Parameters
  ): (Seq[UInt], Seq[Bool], Seq[Bool], EnumBundle) = {
    // Gather inputs
    val dataPathBits: Int = inputBits.head.getWidth
    val A:            UInt = inputBits.head
    val B:            UInt = inputBits(1)
    // Create input valid signal
    val inputValid: Bool = VecInit(inputValids).asUInt().andR()
    // Swtich between ASIC implementation or FPGA backend
    if (isFPGA) {
      // For now only 64-bit double Compare supported
      require(opDataTypeSet.forall(x => x.dataType.get.compBits == 64 && x.dataType.get.unitBits == 64))
      require(A.getWidth == 64 && B.getWidth == 64)
      // Get FPGA IP for FComp
      val fcomp: FComp_D64 = Module(new FComp_D64())
      // Connect clock
      fcomp.io.aclk := clock.asBool()
      // Connect input operands and valid
      fcomp.io.s_axis_a_tdata := A
      fcomp.io.s_axis_a_tvalid := inputValids.head
      fcomp.io.s_axis_b_tdata := B
      fcomp.io.s_axis_b_tvalid := inputValids(1)
      // Connect output ready
      fcomp.io.m_axis_result_tready := outputReadys.head
      // Rearrange Output bits to (gt, eq, lt)
      val outputBits: UInt = WireInit(0.U(64.W))
      outputBits := Cat(
        fcomp.io.m_axis_result_tdata(2),
        fcomp.io.m_axis_result_tdata(0),
        fcomp.io.m_axis_result_tdata(1)
      )
      // Construct input ready signal and exception
      val inputReady: Bool = Mux(inputValid, fcomp.io.s_axis_a_tready && fcomp.io.s_axis_b_tready, false.B)
      val excp:       EnumBundle = Wire(new EnumBundle(Set.empty))
      // Return
      (Seq(outputBits), Seq(fcomp.io.m_axis_result_tvalid), Seq(inputReady, inputReady), excp)
    } else {
      // Loop over all opcode to produce result
      val outputBitsExcpLut: Seq[(UInt, (UInt, UInt))] = opDataTypeSet.unitBitsSet.toSeq.flatMap { unitBits =>
        // Find all OperationDataType belongs to this unitBits
        val OPTs: Set[DsaOperDataType] = opDataTypeSet.filter(opt => opt.unitBits.get == unitBits)
        // Calculate the maximum vecWidth
        val maxVecWidth: Int = OPTs.maxVecWidth
        val compFNs:     Seq[CompareRecFN] = Seq.fill(maxVecWidth)(Module(new CompareRecFN(exp(unitBits), sig(unitBits))))
        compFNs.zipWithIndex.foreach { case (compFN, vecIdx) =>
          val iA: UInt = A((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val iB: UInt = B((vecIdx + 1) * unitBits - 1, vecIdx * unitBits)
          val fA: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iA)
          val fB: UInt = recFNFromFN(exp(unitBits), sig(unitBits), iB)
          compFN.io.a := fA
          compFN.io.b := fB
          compFN.io.signaling := false.B
        }
        // Loop over all operation + dataType under this unitBits
        val partialBitsExcpLut: Seq[(UInt, (UInt, UInt))] = OPTs.toSeq.map {
          case opt @ DsaOperDataType(operation, dataType, _) =>
            val vecWidth: Int = dataType.get.vecWidth
            val invalids: ListBuffer[Bool] = ListBuffer()
            val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
              val iRwire: UInt = WireDefault(0.U(unitBits.W))
              if (vecIdx < vecWidth) {
                iRwire := (operation match {
                  case FloatCompare =>
                    val gt: Bool = compFNs(vecIdx).io.gt
                    val eq: Bool = compFNs(vecIdx).io.eq
                    val lt: Bool = compFNs(vecIdx).io.lt
                    Cat(gt, eq, lt)
                  case err => require(requirement = false, s"Operation $err is not supported by $this"); 0.U
                })
                invalids += compFNs(vecIdx).io.exceptionFlags(4) // in berkeley-hardfloat, only invalid for compare
              }
              iRwire
            }
            encoding(opt).U -> (
              (
                VecInit(vecBits).asUInt(), /* Merge result bits*/
                VecInit(invalids.result()).asUInt().orR().asUInt()
              ) /*any vec element is invalid mean invalid*/
            )
        }
        partialBitsExcpLut
      }
      // Spilt bits and exception
      val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._1)
      val outputExpsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._2)
      // Output
      val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
      val inputReady:     Bool = Mux(inputValid, VecInit(outputReadys).asUInt().andR(), false.B)
      val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
      outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
      val exception: EnumBundle = Wire(
        new EnumBundle(
          Set(InvalidOperation), // Floating Point General Compare
          EnumEncodeMethod.maskEncode,
          supportInvalid = false,
          isOneHot = true
        )
      )
      exception.enumHardwareField.get := MuxLookup(opcode, false.B, outputExpsLut).asUInt()
      val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
      (Seq(outputBits), Seq(outputValid), Seq(inputReady, inputReady), exception)
    }
  }
}
