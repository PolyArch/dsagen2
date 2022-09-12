package dsagen2.top.config

import chipsalliance.rocketchip.config.{Config, Field, Parameters}

case object DebugKey extends Field[Boolean](false)

class WithDebuggable
    extends Config((site, here, up) => { case DebugKey =>
      sys.env.get("DEBUG") match {
        case Some(value) => if (value == "0") false else true
        case None        => false
      }
    })

trait DebugPrintable {
  implicit val p: Parameters;
  val printDebug: Boolean = p(DebugKey)
}
