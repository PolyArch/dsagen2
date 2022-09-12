package dsagen2.ctrl.module

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.reconf.CompReconfProto
import dsagen2.ctrl.config.CtrlKeys.CtrlNode
import dsagen2.ctrl.config.StrDispParams
import dsagen2.top.diplomacy.{DSANodeModule, ReconfNode}
import dsagen2.top.module.DSAGen
import freechips.rocketchip.diplomacy.LazyModule

class StreamDispatcherModule(val dsa: DSAGen)(implicit p: Parameters) extends LazyModule {
  suggestName(s"StreamDispatcher")

  // Controller Parameter
  val ctrlParam:     StrDispParams = p(CtrlNode)
  val dsaNodeModule: DSANodeModule = new CtrlNodeModule

  // Stating point of reconfiguration node
  val initReconfNode: ReconfNode = ReconfNode(
    dsaModule = dsaNodeModule,
    pFn = { _ => CompReconfProto(dsaNodeModule) },
    rFn = { _ => CompReconfProto(dsaNodeModule) }
  )

  lazy val module = new StreamDispatcherImpl(this, dsa)
}
