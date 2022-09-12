package dsagen2.comp.impl

import chisel3.SyncReadMem.WriteFirst
import chisel3._
import chisel3.util._
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.processing_element.PERegFileParameters
import dsagen2.top.config.DSAFixedConfig._

trait RegisterFileImpl {
  val compNode: CompNodeParameters
  val regParam: Option[PERegFileParameters]

  // This is the number of logical register
  def numReg: Int = if (regParam.isDefined) regParam.get.numReg else 0

  def regIdxBits: Int = if (supportReg && numReg > 1) log2Ceil(numReg) else 0

  def regBits: Int = {
    require(compNode.compBits % 2 == 0, s"Compute Bits ${compNode.compBits} is not multiple of 2")
    require(isPow2(compNode.compBits), s"Compute Bits ${compNode.compBits} is not power of 2")
    compNode.compBits
  }

  // This is the number of physical register per logical register
  // Maximum bits per physical register is limited by COMP_CFG_DATA_BITS(32)
  private def numPhyReg: Int = {
    val numPhy: Int = if (regBits > CONF_RF_MAX_BITS) {
      regBits / CONF_RF_MAX_BITS + {
        if (regBits % CONF_RF_MAX_BITS == 0) 0 else 1
      }
    } else 1
    require(numPhy >= 1, s"Number of physical register should not be less than 1, but $numPhy")
    require(isPow2(numPhy), s"The number of physical register per logical register should be power of two")
    numPhy
  }

  def phyRegIdxBits: Int = log2Ceil(numPhyReg * numReg)

  def phyRegOffsetBits: Int = log2Ceil(numPhyReg)

  // Number of read / write port
  def numRegRead: Int // In PE, this should be the maximum number of operand

  def numRegWrite: Int // In PE, this should be the maximum number of result

  /* Virtual IO */

  // Read the register file, inward direction, valid is read enable
  val regReadsValid: Option[Vec[Bool]]
  val regReadsIdx:   Option[Vec[UInt]]
  val regReadsData:  Option[Vec[ValidIO[UInt]]]

  // Write to the register file, inward direction, valid is write enable, bits is register number
  val regWritesValid: Option[Vec[Bool]]
  val regWritesIdx:   Option[Vec[UInt]]
  val regWritesData:  Option[Vec[UInt]]

  // Register File Update Port, since every time we can only write up to 48 bits from config port
  // update to the register file is done in the granularity of physical register
  val regUpdValid: Option[Bool]
  val regUpdIdx:   Option[UInt]
  val regUpdData:  Option[UInt]

  // Reset Register (reset all value to be zero)
  val resetRF: Option[Bool]

  // Actual Logics that connect all virtual IOs
  def registerFile(): Unit = {
    if (regParam.isDefined) {
      val syncRF: Boolean = !regParam.get.asyncRF
      // Create Register File Block
      val rf: MemBase[UInt] = {
        // For each physical register, it can be [[COMP_RF_MAX_BITS]] bit wide
        if (syncRF) SyncReadMem(numReg * numPhyReg, UInt((CONF_RF_MAX_BITS.min(regBits)).W), WriteFirst)
        else Mem(numReg * numPhyReg, UInt(CONF_RF_MAX_BITS.min(regBits).W))
      }
      // Create state to reset the whole resgister file
      val resetting:   Bool = RegInit(false.B)
      val resetRegIdx: Counter = Counter(numReg)

      // Read
      regReadsIdx.getOrElse(Seq.fill(numRegRead)(0.U(1.W))).zip(regReadsData.get).zip(regReadsValid.get).foreach {
        case ((readIdx, readData), readValid) =>
          if (numPhyReg == 1) {
            readData.bits := rf.read(readIdx)
          } else {
            // There are more than one physical register that belongs to one logical register
            // we have to read from all of them
            val bitsPhy: Seq[UInt] = for (phyIdx <- 0 until numPhyReg) yield {
              rf.read(Cat(readIdx, phyIdx.U(phyRegOffsetBits.W)))
            }
            readData.bits := VecInit(bitsPhy).asUInt()
          }
          if (syncRF) {
            // Synchromized memory has one cycle read latency
            readData.valid := RegNext(readValid)
          } else {
            // Combinational memory can do zero cycle write
            readData.valid := readValid
          }
      }

      // Write
      regWritesIdx.getOrElse(Seq.fill(numRegWrite)(0.U(1.W))).zip(regWritesData.get).zip(regWritesValid.get).foreach {
        case ((writeIdx, writeData), writeValid) =>
          if (numPhyReg == 1) {
            when(writeValid || resetting) {
              rf.write(Mux(resetting, resetRegIdx.value, writeIdx), Mux(resetting, 0.U, writeData))
            }
          } else {
            // The write bits is wider than COMP_RF_MAX_BITS
            require(writeData.getWidth > CONF_RF_MAX_BITS)
            when(writeValid || resetting) {
              for (phyIdx <- 0 until numPhyReg) {
                // Write to all of the physical data
                rf.write(
                  Mux(
                    resetting,
                    Cat(resetRegIdx.value, phyIdx.U(phyRegOffsetBits.W)),
                    Cat(writeIdx, phyIdx.U(phyRegOffsetBits.W))
                  ),
                  Mux(resetting, 0.U, writeData((phyIdx + 1) * CONF_RF_MAX_BITS - 1, phyIdx * CONF_RF_MAX_BITS))
                )
              }
            }
          }
      }

      // Update
      if (supportUpdateRegister) {
        (regUpdValid, regUpdIdx, regUpdData) match {
          case (Some(updValid), _, Some(updData)) =>
            val updIdx: UInt = regUpdIdx.getOrElse(0.U(1.W))
            when(updValid) {
              rf.write(updIdx, updData)
            }
          case _ =>
            require(
              requirement = false,
              "I don't think this would happen. It means that we support " +
                "register update but none of related wire was found?"
            )
        }
      }

      // Reset FSM
      when(resetRF.get) {
        resetting := true.B
      } // Kick start the FSM
      when(resetting) {
        when(resetRegIdx.inc()) {
          resetting := false.B
        }
      } // Turn off reset register

      // Sanity check
      require(
        regParam.get.numResetReg <= numRegWrite,
        s"Register Reset is done by write port of register file, you have $numRegWrite write port but" +
          s" you have ${regParam.get.numResetReg} resettable register, which is not allowed"
      )
    } // Judge for definition of register
  }

  // Support Register File
  def supportReg: Boolean = regParam.isDefined && numReg > 0

  // Support Update Register File
  def supportUpdateRegister: Boolean = supportReg && regParam.get.update
}
