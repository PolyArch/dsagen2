package dsagen2.top.diplomacy

import dsagen2.top.diplomacy.DSANodeType.DSAGenNodeType

trait WithNodeIDParameters {
  // Node ID Per Node Type, assign during construction
  // Please use getNodeId
  val nodeId:   Int
  val nodeType: DSAGenNodeType

  /* ---------- Method ---------- */
  def getNodeName: String = {
    require(nodeId >= 0, s"Node ID = $nodeId, should be positive, this means the it is not initialized")
    nodeType.toString + "." + nodeId.toString
  }

  def getNodeId: Int = {
    require(nodeId >= 0, s"Node ID = $nodeId, should be positive, this means the it is not initialized")
    nodeId
  }
}
