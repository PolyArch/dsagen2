package dsagen2.util

import dsagen2.top.diplomacy.DSANodeType.DSAGenNodeType

object NodeUtil {
  // Fail utility when generation
  def failGen(nodeType : DSAGenNodeType, err : String = "") : Unit = {
    val message : String = if(err != "") err else s"$nodeType is not support for VP generation"
    require(requirement = false, message)
  }
}
