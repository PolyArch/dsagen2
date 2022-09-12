package dsagen2.comp.impl.fu.trigonometric

import chipsalliance.rocketchip.config
import chisel3._
import chisel3.util._
import dsagen2.comp.impl.IsFunctionUnitImplementation
import dsagen2.top.bundle.EnumBundle
import dsagen2.top.config.DSAFixedConfig.TRIG_IN_BITS
import dsagen2.top.config.operation.DataType.FixedGroup
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import dsagen2.top.config.operation.OperDataTypeSet.setOpDataTypeEnhance
import dsagen2.top.config.operation.{DataType, OperDataType, Operation}

object Sine extends IsFunctionUnitImplementation {
  val Pi: Double = math.Pi
  // Because it is related to the size of ROM, TODO: do we want to expose this to the DSE?
  val inBits: Int = TRIG_IN_BITS // This is the precision of Sine Table, I made it fixed for now
  /*val outBits : Int = 8*/
  // This is related to operation granularity now
  val supportOperation: collection.Set[Operation.DsaOperation] = Set(Operation.FixedSine)
  val supportDataType:  collection.Set[DataType.DsaDataType] = FixedGroup
  val latency:          Int = 0
  val throughput:       Int = 1

  // This table generate ROM that stores function value with input (0, 90), 0 and 90 are inclusive
  def sinTable(inBits: Int, outBits: Int): Vec[UInt] = {
    // Calculate the max number of input
    require(inBits >= 1, s"inputBits of Sine Table should be more than one")
    val numRow:   Int = 1 << inBits
    val interval: Double = (Pi / 2) / (numRow - 1)
    val times:    Seq[Double] = (0 until numRow).map { i => i * interval }
    val maxOut:   BigInt = (BigInt(1) << outBits) - BigInt(1)
    val resSeq:   Seq[BigInt] = times.map(t => BigInt(math.round(math.sin(t) * (maxOut / 2).toLong)) * 2)
    VecInit(resSeq.map(_.U(outBits.W)))
  }

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
    // Gather IO and sine lookup table
    val dataPathBits: Int = inputBits.head.getWidth
    val X:            UInt = inputBits.head
    // Generate Sine Table per granularity
    val unitBits2sinTab: Map[Int, Vec[UInt]] = opDataTypeSet.unitBitsSet.map { unitBits =>
      unitBits -> sinTable(inBits, unitBits) // #input is fixed to be 8-bit, 0.3 degree as minimum difference
    // outputBits is equal to the granularity
    // inputBits that is higher than 8-bit is discard
    }.toMap
    // Output Bits Lookup
    val outputBitsLut: Seq[(UInt, UInt)] = opDataTypeSet.toSeq.map {
      case opt @ DsaOperDataType(operation, Some(dataType), _) =>
        // Sanity Check
        require(operation == Operation.FixedSine, s"$this only support $supportOperation")
        // Extract parameters
        val vecWidth: Int = dataType.vecWidth
        val unitBits: Int = dataType.unitBits
        // Generate vectorization result
        val vecBits: Seq[UInt] = (0 until dataPathBits / unitBits).map { vecIdx =>
          val iRwire: UInt = WireDefault(0.U(unitBits.W))
          if (vecIdx < vecWidth) {
            val tab: Vec[UInt] = unitBits2sinTab(unitBits)
            val iX: UInt = {
              // If granularity is larger than LUT inputBits, then use the lower
              if (unitBits >= inBits) {
                X(vecIdx * unitBits + inBits - 1, vecIdx * unitBits)
              } else {
                // If granularity is smaller than LUT inputBits, then use it as address higher part
                val lowerBits: Int = inBits - unitBits
                val sX:        UInt = X(vecIdx * unitBits + unitBits - 1, vecIdx * unitBits)
                Cat(sX, Fill(lowerBits, 0.U(1.W)))
              }
            }
            require((1 << iX.getWidth) == tab.length, s"Part of LUT can never be accessed")
            val result: UInt = tab(iX)
            require(result.getWidth == iRwire.getWidth, s"Result bitWidth mismatch")
            iRwire := result
          }
          iRwire
        }
        encoding(opt).U -> VecInit(vecBits).asUInt()
      case err =>
        require(requirement = false, s"Operation $err is not supported by $this")
        -1.U -> -1.U
    }
    // Output
    val outputValid:    Bool = VecInit(inputValids).asUInt().andR()
    val inputReady:     Bool = Mux(outputValid, VecInit(outputReadys).asUInt().andR(), false.B)
    val outputBitsWire: UInt = WireDefault(0.U(dataPathBits.W))
    outputBitsWire := MuxLookup(opcode, 0.U, outputBitsLut)
    val outputBits: UInt = Mux(outputValid, outputBitsWire, 0.U)
    (Seq(outputBits), Seq(outputValid), Seq(inputReady), WireDefault(0.U.asTypeOf(new EnumBundle(Set.empty))))
  }
}
