package dsagen2.top.diplomacy

import dsagen2.top.diplomacy.DSANodeType.{DSAGenNodeType, Switch}

/** DSA Node Module is a module with node type, id and a CDE parameter
  */
trait DSANodeModule {
  val nodeId:   Int
  val nodeType: DSANodeType.DSAGenNodeType

  override def toString: String = nodeType.toString + "." + nodeId
}

class NoNodeModule(val nodeType: DSAGenNodeType = Switch, val nodeId: Int = -1) extends DSANodeModule
