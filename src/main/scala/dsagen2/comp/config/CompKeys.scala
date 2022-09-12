package dsagen2.comp.config

import chipsalliance.rocketchip.config.Field
import dsagen2.comp.config.common.CompNodeOutputBufferParameters
import dsagen2.comp.config.processing_element._
import dsagen2.comp.config.switch.SWRoutingParameters
import dsagen2.top.config.{JsonParsableKey, ReconfKey}

case object CompKeys {
  /* ---------- Keys Group ---------- */
  case object BaseKeys {
    val keys: Seq[JsonParsableKey] = Seq(CompNode, OutputBuffer)
  }

  case object PEKeys {
    val allKeys:    Seq[JsonParsableKey] = BaseKeys.keys ++ Seq(DsaOperations, RegFile, MetaControl)
    val reconfKeys: Seq[ReconfKey] = Seq(DsaOperations, MetaControl)
  }

  case object SWKeys {
    val allKeys:    Seq[JsonParsableKey] = BaseKeys.keys ++ Seq(SwitchRouting)
    val reconfKeys: Seq[ReconfKey] = Seq(SwitchRouting)
  }

  /* ---------- Common ---------- */

  // non-reconfigure
  case object CompNode extends Field[CompNodeParameters] with JsonParsableKey // #input/#output

  // non-reconfigure,
  case object OutputBuffer
      extends Field[CompNodeOutputBufferParameters]
      with JsonParsableKey // WithNBufferModule, finished

  /* ---------- Processing Element ---------- */

  // reconfigurable
  // input sel, input ctrl sel, ctrl mode, delay sel, fu opcode, res desel, inst delay, inst repeat
  case object DsaOperations
      extends Field[PEDsaOperationParameters]
      with ReconfKey
      with JsonParsableKey // WithAluModule, TODO

  // non-configurable
  // XLEN bit, number of register
  case object RegFile extends Field[PERegFileParameters] with JsonParsableKey // WithRegisterFile

  // reconfigurable
  // LUT based meta control
  case object MetaControl
      extends Field[PEMetaCtrlParameters]
      with ReconfKey
      with JsonParsableKey // WithMetaControl, TODO

  /* ---------- Switch ---------- */

  // reconfigurable
  case object SwitchRouting
      extends Field[SWRoutingParameters]
      with ReconfKey
      with JsonParsableKey // WithRoutingModule, finished
}
