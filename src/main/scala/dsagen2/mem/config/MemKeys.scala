package dsagen2.mem.config

import chipsalliance.rocketchip.config.Field
import dsagen2.top.config.JsonParsableKey

object MemKeys {
  /* ---------- Common ---------- */
  case object MemNode extends Field[MemNodeParameters] with JsonParsableKey
}
