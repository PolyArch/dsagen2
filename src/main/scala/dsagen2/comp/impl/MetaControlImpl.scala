package dsagen2.comp.impl

import chisel3._
import chisel3.util._
import dsagen2.comp.bundle.PECtrlEntry
import dsagen2.comp.config.processing_element.{PEDsaOperationParameters, PEMetaCtrlParameters, PERegFileParameters}

/** There are three function of the meta control: reset register to zero, reuse operands, discard results
  * So the Interface is like:
  *
  * Control Port of Meta Control
  * -- Key from ALU : UInt
  * -- Vector of Meta Control Entry
  *
  * Reset Register:
  * -- Results from ALU : Vec[ DecoupledIO[UInt] ]
  * -- Modified results to Register File : Vec[ DecoupledIO[UInt] ]
  *
  * Reuse Operands:
  * -- Ready from ALU : Vec[Bool]
  * -- Modified ready to Delay FIFOs : Vec[Bool]
  *
  * Discard Results
  * -- Valid from ALU : Vec[Bool]
  * -- Modified Valid to Register File / Output Muxes
  *
  * Abstain: Do nothing, pull high all reuse operand, invalidate all result, do not reset register
  */
trait MetaControlImpl {
  val aluParam:  PEDsaOperationParameters
  val ctrlParam: Option[PEMetaCtrlParameters]
  val regParam:  Option[PERegFileParameters]

  /* ----- Virtual IO ----- */
  // Meta Control Input
  val ctrlKey: Option[ValidIO[UInt]]
  val metaLUT: Option[Vec[PECtrlEntry]]
  // Meta Control Output
  val regReset:  Option[UInt]
  val operReuse: Option[UInt]
  val resultDis: Option[UInt]
  val abstain:   Option[Bool]

  /* ----- Construct Logics ----- */

  // Control is done by an LUT
  def control(): Unit = {
    if (supportCtrl) {
      require(ctrlKey.isDefined)
      require(metaLUT.isDefined)
      val selectEntry:      PECtrlEntry = metaLUT.get.apply(ctrlKey.get.bits)
      val abstainOverwrite: Bool = abstain.getOrElse(false.B)
      when(ctrlKey.get.valid && selectEntry.valid.get) {
        regReset match {
          case Some(reset) =>
            require(selectEntry.regReset.isDefined)
            require(selectEntry.regReset.get.getWidth == reset.getWidth)
            // If it is overwritten by abstain, do not reset register
            reset := Mux(abstainOverwrite, 0.U, selectEntry.regReset.get)
          case None =>
        }
        operReuse match {
          case Some(reuse) =>
            require(selectEntry.operReuse.isDefined)
            require(selectEntry.operReuse.get.getWidth == reuse.getWidth)
            // If it is overwritten by abstain, then reuse all operands
            reuse := Mux(abstainOverwrite, Fill(reuse.getWidth, 1.U(1.W)), selectEntry.operReuse.get)
          case None =>
        }
        resultDis match {
          case Some(discard) =>
            require(selectEntry.resultDis.isDefined)
            require(selectEntry.resultDis.get.getWidth == discard.getWidth)
            // if it if overwritten by obstain, then discard all results
            discard := Mux(abstainOverwrite, Fill(discard.getWidth, 1.U(1.W)), selectEntry.resultDis.get)
          case None =>
        }
        abstain match {
          case Some(abstain) => abstain := selectEntry.abstain.get
          case None          =>
        }
      }.otherwise {
        regReset match {
          case Some(reset) => reset := 0.U
          case None        =>
        }
        operReuse match {
          case Some(reuse) => reuse := 0.U
          case None        =>
        }
        resultDis match {
          case Some(discard) => discard := 0.U
          case None          =>
        }
        abstain match {
          case Some(abstain) => abstain := false.B
          case None          =>
        }
      }
    } else {
      require(metaLUT.isEmpty)
      require(ctrlKey.isEmpty)
      require(regReset.isEmpty)
      require(operReuse.isEmpty)
      require(resultDis.isEmpty)
      require(abstain.isEmpty)
    }
  }

  // Sanity check

  if (supportResetReg) {
    require(regParam.isDefined)
    require(regParam.get.numResetReg > 0)
  }

  if (supportReuseOper) {
    require(aluParam.maxNumOperand > 0)
  }

  if (supportDiscard) {
    require(aluParam.maxNumResult > 0)
  }

  // Support Control
  def supportCtrl: Boolean = ctrlParam.isDefined && ctrlParam.get.supportControl

  // Support Input Control
  def supportInputCtrl: Boolean = ctrlParam.isDefined && ctrlParam.get.inputCtrl

  // Support Output Control
  def supportOutputCtrl: Boolean = ctrlParam.isDefined && ctrlParam.get.outputCtrl

  // Support Reset Register
  def supportResetReg: Boolean = supportCtrl && ctrlParam.get.resetReg

  // Support Reuse Operands
  def supportReuseOper: Boolean = supportCtrl && ctrlParam.get.reuseOperand

  // Support Discard Result
  def supportDiscard: Boolean = supportCtrl && ctrlParam.get.discardResult

  // Support Abstain
  def supportAbstain: Boolean = supportCtrl && ctrlParam.get.abstain
}
