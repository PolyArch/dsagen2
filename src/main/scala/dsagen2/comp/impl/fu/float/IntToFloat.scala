package dsagen2.comp.impl.fu.float

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util.MuxLookup
import dsagen2.comp.config.exception.CompException.floatException
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FloatGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet._
import dsagen2.top.config.operation.Operation._
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}
import hardfloat.INToRecFN

import scala.collection.Set

object IntToFloat extends IsFunctionUnitImplementation {
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(
    // Unsigned Integer to Floating
    UInt8ToHalf16,
    UInt8ToFloat32,
    UInt8ToDouble64,
    UInt16ToHalf16,
    UInt16ToFloat32,
    UInt16ToDouble64,
    UInt32ToHalf16,
    UInt32ToFloat32,
    UInt32ToDouble64,
    UInt64ToHalf16,
    UInt64ToFloat32,
    UInt64ToDouble64,
    // Signed Integer to Floating
    SInt8ToHalf16,
    SInt8ToFloat32,
    SInt8ToDouble64,
    SInt16ToHalf16,
    SInt16ToFloat32,
    SInt16ToDouble64,
    SInt32ToHalf16,
    SInt32ToFloat32,
    SInt32ToDouble64,
    SInt64ToHalf16,
    SInt64ToFloat32,
    SInt64ToDouble64
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
    val i2f2FnOutEcp: Map[(Int, Int), (Seq[UInt], Seq[EnumBundle])] = opDataTypeSet.operations.map { x =>
      val (sourceDT, sinkDT, isConversion) = x.sourceSinkDT
      require(isConversion, s"Operation $x is not data Type conversion")
      require(sourceDT.isFixed && sinkDT.isFloat, s"This is fixed to float data type conversion, but input is not")
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
      val i2fFNs: Seq[INToRecFN] =
        Seq.fill(maxVecWidth)(Module(new INToRecFN(sourceBits, exp(sourceBits), sig(sourceBits))))
      // Connect input to each Float2Int module
      i2fFNs.zipWithIndex.foreach { case (i2fFN, vecIdx) =>
        // Collect vector element unitBits of this vecIdx
        val elemBitsSignLut: Seq[(UInt, (UInt, Bool))] =
          targetOps.filter(opt => vecIdx < opt.dataType.get.vecWidth).toSeq.map { opt =>
            val unitBits: Int = opt.unitBits.get
            val enc:      UInt = encoding(opt).U
            val signedIn: Bool = {
              opt.operation match {
                case op if op.sourceSinkDT._1.isSigned =>
                  require(op.sourceSinkDT._2.isFloat, s"operation $op's sink is not floating")
                  true.B
                case op if op.sourceSinkDT._1.isUnsigned =>
                  require(op.sourceSinkDT._2.isFloat, s"operation $op's sink is not floating")
                  false.B
                case err =>
                  require(requirement = false, s"Operation $err's source is floating or else")
                  false.B
              }
            }
            enc -> ((source(vecIdx * unitBits + sourceBits - 1, vecIdx * unitBits), signedIn))
          }
        // Split the bits with the signed
        val elemBitsLut: Seq[(UInt, UInt)] = elemBitsSignLut.map(x => x._1 -> x._2._1)
        val elemSignLut: Seq[(UInt, UInt)] = elemBitsSignLut.map(x => x._1 -> x._2._2)
        // MuxLookup and convert to recorded Float Number
        val iElem: UInt = MuxLookup(opcode, 0.U, elemBitsLut)
        require(iElem.getWidth == sourceBits, s"Source Integer's bitWidth is not equla to given $sourceBits")
        i2fFN.io.signedIn := MuxLookup(opcode, false.B, elemSignLut)
        i2fFN.io.in := iElem
        i2fFN.io.roundingMode := hardfloat.consts.round_near_even
        i2fFN.io.detectTininess := hardfloat.consts.tininess_beforeRounding
      }
      // Return with (sourceBits, sinkBits) ->
      (sourceBits, sinkBits) -> (
        (
          i2fFNs.map(_.io.out),
          i2fFNs.map(_.io.exceptionFlags.asTypeOf(new EnumBundle(floatException)))
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
        val i2fFNs:     Seq[UInt] = i2f2FnOutEcp((sourceBits, sinkBits))._1
        val exceptions: Seq[EnumBundle] = i2f2FnOutEcp((sourceBits, sinkBits))._2
        // Get the input/output vectorization and granularity
        val unitBits: Int = dataType.unitBits
        val vecWidth: Int = dataType.vecWidth
        // Collect Result
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          // Element Result wire
          val iRwire: UInt = WireDefault(0.U(unitBits.W))
          if (vecIdx < vecWidth) iRwire := i2fFNs(vecIdx)
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
    val exception: EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(floatException)))
    EnumBundle.connect(exception, MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(floatException)))
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    (Seq(outputBits), Seq(outputValid), Seq(inputReady), exception)
  }
}
