package dsagen2.sync.config

import chipsalliance.rocketchip.config.Field
import dsagen2.top.config.JsonParsableKey

case object SyncKeys {

  /* ---------- Keys to Vector Port ---------- */
  case object IVPNode extends Field[IVPNodeParameters] with JsonParsableKey

  case object OVPNode extends Field[OVPNodeParameters] with JsonParsableKey
}
