package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Config
import dsagen2.comp.config.CompKeys._

/** There are three kinds of Meta Control, Input-bit control, Output-bit control, and routed Input bits (stream state)
  * control
  *
  * @param inputLSBCtrl  Select one of input ports and use its LSB bits as control key
  * @param outputLSBCtrl Select one of output ports and use its LSB bits as control key
  * @param keyBits       The number of bits used to access the LUT, We need three bit for compare result and 6-bit for stream state
  * @param abstain       support do nothing in FU
  * @param resetRegister Meta control support reset register
  * @param reuseOperand  Meta control support reuse operand
  * @param discardResult Meta control support discard result
  * @param sizeLUT       The number of entry of Meta Control Look Up Table
  */
class WithMetaControl( // Input/Output Controlled
  inputLSBCtrl:  Boolean = true,
  outputLSBCtrl: Boolean = true,
  // Meta Controlled Function
  abstain:       Boolean = true,
  resetRegister: Boolean = true,
  reuseOperand:  Boolean = true,
  discardResult: Boolean = true,
  // Size of Meta LUT
  sizeLUT: Int = 4)
    extends Config((site, here, up) => {
      // Check pre defined parameters
      case MetaControl =>
        // Look up for old parameters
        up.lift(MetaControl) match {
          // Old parameters
          case Some(old) =>
            old.copy(
              inputLSBCtrl, // update the input controlled of original parameter
              outputLSBCtrl, // update the output controlled of original parameter
              abstain,
              resetRegister, // update whether or not support update register
              reuseOperand, // update whether or not support reuse operand
              discardResult, // update whether or not support discard result
              sizeLUT // the size of LUT
            )
          // Define new parameters
          case None =>
            PEMetaCtrlParameters( //
              inputLSBCtrl,
              outputLSBCtrl,
              abstain,
              resetRegister,
              reuseOperand,
              discardResult,
              sizeLUT
            )
        }
    })
