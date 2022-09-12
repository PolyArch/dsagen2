package dsagen2.ctrl.module

import dsagen2.top.diplomacy.DSANodeModule
import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, StreamDispatcher}

class CtrlNodeModule extends DSANodeModule {
  val nodeId:   Int = 0
  val nodeType: DSAGenNodeType = StreamDispatcher
}
