package dsagen2.comp.impl.fu.fixed

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util._
import dsagen2.comp.config.exception.CompException.FixedOverflow
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.Operation.DsaOperation
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

import scala.collection.mutable.ListBuffer

object Int2Int extends IsFunctionUnitImplementation {
  override val supportOperation: collection.Set[Operation.DsaOperation] = {
    val allOperation: Set[DsaOperation] = Operation.values.map(_.asInstanceOf[DsaOperation])
    allOperation.filter { op =>
      val (sourceDT, sinkDT, isConversion) = op.sourceSinkDT
      sourceDT.isFixed && sinkDT.isFixed && isConversion
    }
  }
  override val supportDataType: collection.Set[DataType.DsaDataType] = FixedGroup
  override val latency:         Int = 0
  override val throughput:      Int = 1

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
    val A:            UInt = inputBits.head
    // Generate output Bits and exception
    val outputBitsExcpLut: Seq[(UInt, (UInt, UInt))] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(operation, Some(dataType), _) =>
        // Extract parameter
        val (sourceDT, sinkDT, isConversion) = operation.sourceSinkDT
        val sourceBits:   Int = sourceDT.unitBits
        val sinkBits:     Int = sinkDT.unitBits
        val sourceSigned: Boolean = sourceDT.isSigned
        val sinkSigned:   Boolean = sinkDT.isSigned
        val vecWidth:     Int = dataType.vecWidth
        val unitBits:     Int = dataType.unitBits
        // Some Sanity Check
        require(
          sourceDT.isFixed && sinkDT.isFixed && isConversion,
          s"Operation $opt is not fixed to fixed conversion operation"
        )
        require(
          unitBits >= sourceBits && unitBits >= sinkBits,
          s"The granularity of operation should be wider than data type itself"
        )
        // Generate outputBits and exception
        val exceptions: ListBuffer[EnumBundle] = ListBuffer()
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iRwire: UInt = WireDefault(0.U(unitBits.W))
          if (vecIdx < vecWidth) {
            val iSource: UInt = A(vecIdx * unitBits + sourceBits - 1, vecIdx * unitBits)
            val iSink:   UInt = Wire(UInt(sinkBits.W))
            val iEwire:  EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(Set(FixedOverflow))))
            // Actual Conversion of DataType
            (sourceSigned, sinkSigned) match {
              case (true, true) => // SInt -> SInt
                if (sourceBits > sinkBits) {
                  // like SInt32 -> SInt8, detect overflow and just truncate
                  val highSource: UInt = iSource(sourceBits - 1, sinkBits)
                  val overflow:   Bool = highSource.xorR()
                  iEwire.enumHardwareField.get := overflow.asUInt()
                  iSink := iSource // auto truncate
                } else if (sourceBits < sinkBits) {
                  // like SInt16 -> SInt64, just extend the sign bit
                  val signBit:      Bool = iSource(sourceBits - 1)
                  val extendedSign: UInt = Fill(sinkBits - sourceBits, signBit)
                  iEwire.enumHardwareField.get := 0.U
                  val extendedResult: UInt = Cat(extendedSign, iSource)
                  require(
                    extendedResult.getWidth == iSink.getWidth,
                    s"Different Width assignment, " +
                      s"extendedResult[${extendedResult.getWidth}] != iSink[${iSink.getWidth}]"
                  )
                  iSink := extendedResult
                } else {
                  require(requirement = false, "I don't same sign and same width you need conversion")
                }
              case (true, false) => // SInt -> UInt
                if (sourceBits > sinkBits) {
                  // like SInt64 -> UInt32, detect overflow and truncate
                  val highSource: UInt = iSource(sourceBits - 1, sinkBits)
                  val overflow:   Bool = highSource.orR() // higher part need to be all zero if not overflow
                  iEwire.enumHardwareField.get := overflow.asUInt()
                  iSink := iSource // auto truncate
                } else if (sourceBits < sinkBits) {
                  // like SInt16 -> UInt32, if MSB is not one, then just extends with zero, otherwise overflow
                  val overflow:       Bool = iSource(sourceBits - 1)
                  val extendZero:     UInt = Fill(sinkBits - sourceBits, 0.U(1.W))
                  val extendedResult: UInt = Cat(extendZero, iSource)
                  require(extendedResult.getWidth == iSink.getWidth, s"Extended result is not same width of Sink")
                  iEwire.enumHardwareField.get := overflow.asUInt()
                  iSink := extendedResult
                } else {
                  // like SInt8 -> UInt8, MSB is overflow, just assign
                  iEwire.enumHardwareField.get := iSource(sourceBits - 1).asUInt()
                  require(iSink.getWidth == iSource.getWidth)
                  iSink := iSource
                }
              case (false, true) => // UInt -> SInt
                if (sourceBits > sinkBits) {
                  // like UInt64 -> SInt32, non-overflow needs higher and Sink MSB position be all zero
                  val higherSource: UInt = iSource(sourceBits - 1, sinkBits - 1) // sinkBits - 1 includes MSB
                  val overflow:     Bool = higherSource.orR()
                  iEwire.enumHardwareField.get := overflow.asUInt()
                  iSink := iSource // auto truncate
                } else if (sourceBits < sinkBits) {
                  // like UInt16 -> SInt64, just extends with zero, which is auto
                  iSink := iSource
                } else {
                  // like UInt32 -> SInt32
                  iEwire.enumHardwareField.get := iSource(sourceBits - 1).asUInt()
                  iSink := iSource
                }
              case (false, false) => // UInt -> UInt
                if (sourceBits > sinkBits) {
                  // like UInt32 -> UInt8, detect overflow and just truncate
                  val higherSource: UInt = iSource(sourceBits - 1, sinkBits)
                  iEwire.enumHardwareField.get := higherSource.xorR().asUInt()
                  iSink := iSource
                } else if (sourceBits < sinkBits) {
                  // like UInt16 -> UInt64, just extend the sign bit
                  iSink := iSource
                } else {
                  require(requirement = false, "I don't same sign and same width you need conversion")
                }
            }
            iRwire := iSink
            exceptions += iEwire
          }
          iRwire
        } // End of vectorization
        // Convert exception to UInt
        val exceptionUInt: UInt = exceptions.reduce(EnumBundle.merge).asUInt()
        // Return with mapping
        encoding(opt).U -> ((VecInit(vecBits).asUInt(), exceptionUInt))
      case err =>
        require(requirement = false, s"Operation $err is not supported")
        -1.U -> ((-1.U, -1.U))
    }
    // Split the outputBits and exception
    val outputBitsLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._1)
    val outputExcpLut: Seq[(UInt, UInt)] = outputBitsExcpLut.map(x => x._1 -> x._2._2)
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = WireDefault(0.U(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val exception: EnumBundle = WireDefault(0.U.asTypeOf(new EnumBundle(Set(FixedOverflow))))
    EnumBundle.connect(exception, MuxLookup(opcode, 0.U, outputExcpLut).asTypeOf(new EnumBundle(Set(FixedOverflow))))
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    (Seq(outputBits), Seq(outputValid), Seq(inputReady), exception)
  }
}
