package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.config.exception.CompException.floatException
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.{DsaDataType, FloatGroup}
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import hardfloat.{RecFNToRecFN, fNFromRecFN, recFNFromFN}

import scala.collection.Set
import scala.collection.mutable.ListBuffer

object FloatToFloat extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(
    Half16ToFloat32,
    Half16ToDouble64,
    Float32ToHalf16,
    Float32ToDouble64,
    Double64ToHalf16,
    Double64ToFloat32
  )
  val supportDataType: collection.Set[DataType.DsaDataType] = FloatGroup
  val latency:         Int = 0
  val throughput:      Int = 1

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
    // Gather IO
    val dataPathBits: Int = inputBits.head.getWidth
    val source:       UInt = inputBits.head
    // Create an operation to set of F2F module mapping
    val oper2fus: Map[DsaOperation, Seq[(RecFNToRecFN, UInt)]] = opDataTypeSet.operations.map { op =>
      // The widest vectorization of this operation
      val maxVecWidth: Int = opDataTypeSet.filter(x => x.operation == op).maxVecWidth
      // Filter out the data type of this operation
      val dataTypes: Set[DsaDataType] = opDataTypeSet.filter(x => x.operation == op).dataTypes
      // Extract the source floating data type and sink floating data type
      val (sourceDT, sinkDT, isConversion) = op.sourceSinkDT
      require(isConversion, s"Operation $op is not floating2floating conversion operation")
      // Create Module
      val f2fNs = Seq.fill(maxVecWidth)(
        Module(
          new RecFNToRecFN(exp(sourceDT.unitBits), sig(sourceDT.unitBits), exp(sinkDT.unitBits), sig(sinkDT.unitBits))
        )
      )
      // Connect to the input side of module based
      f2fNs.zipWithIndex.foreach { case (f2fN, vecIdx) =>
        // Collect element compBits of this vecIdx
        val subBitsLut: Seq[(UInt, UInt)] = dataTypes.filter(dt => vecIdx < dt.vecWidth).toSeq.map { dt =>
          val unitBits: Int = dt.unitBits
          val enc:      UInt = encoding(DsaOperDataType(op, Some(dt))).U
          enc -> source(vecIdx * unitBits + sourceDT.unitBits - 1, vecIdx * unitBits)
        }
        // MuxLookUp and convert to recorded FN
        val iS: UInt = MuxLookup(opcode, 0.U, subBitsLut)
        val fS: UInt = recFNFromFN(exp(sourceDT.unitBits), sig(sourceDT.unitBits), iS)
        f2fN.io.in := fS
        f2fN.io.roundingMode := hardfloat.consts.round_near_even
        f2fN.io.detectTininess := hardfloat.consts.tininess_beforeRounding
      }
      // Return
      op -> f2fNs.map(f => (f, fNFromRecFN(exp(sinkDT.unitBits), sig(sinkDT.unitBits), f.io.out)))
    }.toMap
    // Loop over all operation + dataType, opcode -> (bits, exception)
    val outputBitsExcpLut: Seq[(UInt, (UInt, UInt))] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(operation, dataType, _) =>
        // Extract Floating Module
        val f2fNs: Seq[RecFNToRecFN] = oper2fus(operation).map(_._1)
        val fOuts: Seq[UInt] = oper2fus(operation).map(_._2)
        // Extract parameter
        val unitBits: Int = dataType.get.unitBits
        val vecWidth: Int = dataType.get.vecWidth
        // buffer to collect exception signals
        val exceptions: ListBuffer[EnumBundle] = ListBuffer()
        // collect result
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iRwire: UInt = WireDefault(0.U(unitBits.W))
          if (vecIdx < vecWidth) {
            val f2fFn:  RecFNToRecFN = f2fNs(vecIdx)
            val iEwire: EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(floatException)))
            val fO:     UInt = fOuts(vecIdx)
            require(
              fO.getWidth <= iRwire.getWidth,
              s"Vectorization granularity ${iRwire.getWidth}-bit is narrow than result bits ${fO.getWidth}-bit"
            )
            iRwire := fO
            EnumBundle.connect(iEwire, f2fFn.io.exceptionFlags.asTypeOf(new EnumBundle(floatException)))
            exceptions += iEwire
          }
          iRwire
        }
        // Reduce the exception from all elements of vector, cast to UInt and return
        val exceptionUInt: UInt =
          exceptions.result().reduce(EnumBundle.merge).asUInt()
        encoding(opt).U -> ((VecInit(vecBits).asUInt(), exceptionUInt))
    }
    // Spilt the output bits and exception
    val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._1)
    val outputExcpLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._2)
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = Wire(UInt(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val exception: EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(floatException)))
    EnumBundle.connect(exception, MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(floatException)))
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    (Seq(outputBits), Seq(outputValid), Seq(inputReady), exception)
  }
}
