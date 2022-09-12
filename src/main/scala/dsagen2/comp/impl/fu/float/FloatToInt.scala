package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.config.exception.CompException.floatConversionException
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FloatGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import hardfloat.{RecFNToIN, recFNFromFN}

import scala.collection.Set

object FloatToInt extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(
    // Floating to Unsigned Integer
    Half16ToUInt8,
    Half16ToUInt16,
    Half16ToUInt32,
    Half16ToUInt64,
    Float32ToUInt8,
    Float32ToUInt16,
    Float32ToUInt32,
    Float32ToUInt64,
    Double64ToUInt8,
    Double64ToUInt16,
    Double64ToUInt32,
    Double64ToUInt64,
    // Floating to Signed Integer
    Half16ToSInt8,
    Half16ToSInt16,
    Half16ToSInt32,
    Half16ToSInt64,
    Float32ToSInt8,
    Float32ToSInt16,
    Float32ToSInt32,
    Float32ToSInt64,
    Double64ToSInt8,
    Double64ToSInt16,
    Double64ToSInt32,
    Double64ToSInt64
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
    // Create an operation to set of F2I module mapping, unsigned and signed share the same module
    val f2i2FnOutEcp: Map[(Int, Int), (Seq[UInt], Seq[EnumBundle])] = opDataTypeSet.operations.map { x =>
      val (sourceDT, sinkDT, isConversion) = x.sourceSinkDT
      require(isConversion, s"Operation $x is not data Type conversion")
      require(sourceDT.isFloat && sinkDT.isFixed, s"This is float to fixed data type conversion")
      // Take the source and sink granularity out, and because of Set, it will be distinct automatically
      (sourceDT.unitBits, sinkDT.unitBits)
    }.map { case (sourceBits, sinkBits) => // Loop over source -> sink granularity
      // Filter out the operations+dataType with same sourceBits and sinkBits
      val targetOps: Set[DsaOperDataType] = opDataTypeSet.filter { x =>
        val (sourceDT, sinkDT, isConversion) = x.operation.sourceSinkDT
        require(isConversion, s"Operation ${x.operation} is not conversion?")
        sourceDT.unitBits == sourceBits && sinkDT.unitBits == sinkBits
      }
      // Get the max vectorization width of this source -> sink granularity
      val maxVecWidth: Int = targetOps.maxVecWidth
      // Create Float to Integer module
      val f2iFNs: Seq[RecFNToIN] =
        Seq.fill(maxVecWidth)(Module(new RecFNToIN(exp(sourceBits), sig(sourceBits), sinkBits)))
      // Connect input to each Float2Int module
      f2iFNs.zipWithIndex.foreach { case (f2iFN, vecIdx) =>
        // Collect vector element unitBits of this vecIdx
        val elemBitsSignLut: Seq[(UInt, (UInt, Bool))] =
          targetOps.filter(opt => vecIdx < opt.dataType.get.vecWidth).toSeq.map { opt =>
            val unitBits: Int = opt.unitBits.get
            val enc:      UInt = encoding(opt).U
            val signedOut: Bool = {
              opt.operation match {
                case op if op.sourceSinkDT._2.isSigned =>
                  require(op.sourceSinkDT._1.isFloat, s"operation $op's source is not floating")
                  true.B
                case op if op.sourceSinkDT._2.isUnsigned =>
                  require(op.sourceSinkDT._1.isFloat, s"operation $op's source is not floating")
                  false.B
                case err =>
                  require(requirement = false, s"Operation $err's sink is floating or else")
                  false.B
              }
            }
            enc -> ((source(vecIdx * unitBits + sourceBits - 1, vecIdx * unitBits), signedOut))
          }
        // Split the bits with the signed
        val elemBitsLut: Seq[(UInt, UInt)] = elemBitsSignLut.map(x => x._1 -> x._2._1)
        val elemSignLut: Seq[(UInt, UInt)] = elemBitsSignLut.map(x => x._1 -> x._2._2)
        // MuxLookup and convert to recorded Float Number
        val iElem: UInt = MuxLookup(opcode, 0.U, elemBitsLut)
        val fElem: UInt = recFNFromFN(exp(sourceBits), sig(sourceBits), iElem)
        require(iElem.getWidth + 1 == fElem.getWidth, s"Convert to record floating increase by 1 bit")
        require(
          f2iFN.io.in.getWidth == fElem.getWidth,
          s"Bitwidth mismatch fElem[${fElem.getWidth}] => " +
            s"floating module input [${f2iFN.io.in.getWidth}]"
        )
        f2iFN.io.in := fElem
        f2iFN.io.roundingMode := hardfloat.consts.round_near_even
        f2iFN.io.signedOut := MuxLookup(opcode, false.B, elemSignLut)
      }
      // Return with (sourceBits, sinkBits) ->
      (sourceBits, sinkBits) -> (
        (
          f2iFNs.map(_.io.out),
          f2iFNs.map(_.io.intExceptionFlags.asTypeOf(new EnumBundle(floatConversionException)))
        )
      )
    }.toMap
    // Loop over all operation + dataType, opcode -> (bits, exception)
    val outputBitsExcpLut: Seq[(UInt, (UInt, UInt))] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(operation, Some(dataType), _) =>
        // Get the sourceBits and sinkBits
        val (source, sink, isConversion) = operation.sourceSinkDT
        require(isConversion, s"Not an conversion operation")
        val sourceBits: Int = source.unitBits
        val sinkBits:   Int = sink.unitBits
        // Get the vector result and exceptions
        val f2iFNs:     Seq[UInt] = f2i2FnOutEcp((sourceBits, sinkBits))._1
        val exceptions: Seq[EnumBundle] = f2i2FnOutEcp((sourceBits, sinkBits))._2
        // Get the input/output vectorization and granularity
        val unitBits: Int = dataType.unitBits
        val vecWidth: Int = dataType.vecWidth
        // Collect Result
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          // Element Result wire
          val iRwire: UInt = WireDefault(0.U(unitBits.W))
          if (vecIdx < vecWidth) {
            iRwire := f2iFNs(vecIdx)
          }
          iRwire
        }
        // Reduce exception
        val exceptionUInt: UInt = exceptions.reduce(EnumBundle.merge).asUInt()
        // Return
        encoding(opt).U -> ((VecInit(vecBits).asUInt(), exceptionUInt))
      case err =>
        require(requirement = false, s"Operation+DataType $err format is wrong"); -1.U -> ((-1.U, -1.U))
    }
    // Split the outputBits and exception
    val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._1)
    val outputExcpLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._2)
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = WireDefault(0.U(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val exception: EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(floatConversionException)))
    EnumBundle.connect(
      exception,
      MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(floatConversionException))
    )
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    (Seq(outputBits), Seq(outputValid), Seq(inputReady), exception)
  }
}
