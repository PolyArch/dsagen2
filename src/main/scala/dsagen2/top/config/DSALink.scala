package dsagen2.top.config

import dsagen2.top.config.ParseKeys._
import dsagen2.top.diplomacy.DSANodeType.DSAGenNodeType
import play.api.libs.json.{JsObject, Json}

// This is the definition for each directional connection between DSANode
case class DSALink( // Source Node Info
  sourceNodeType: DSAGenNodeType,
  sourceNodeId:   Int,
  sourceEdgeIdx:  Int,
  // Sink Node Info
  sinkNodeType: DSAGenNodeType,
  sinkNodeId:   Int,
  sinkEdgeIdx:  Int) {
  require(sourceNodeId >= 0, s"$sourceNodeType has negative source node id = $sourceNodeId")
  require(sourceEdgeIdx >= 0, s"$sourceNodeType $sourceNodeId has negative source edge index = $sourceEdgeIdx")

  require(sinkNodeId >= 0, s"$sinkNodeType has negative sink node id = $sinkNodeId")
  require(sinkEdgeIdx >= 0, s"$sinkNodeType $sinkNodeId has negative sink edge index = $sinkEdgeIdx")

  def toJSON: JsObject =
    Json.obj(
      DSALinkSourceNodeType.keyName -> sourceNodeType.toString,
      DSALinkSourceNodeId.keyName -> sourceNodeId,
      DSALinkSourceIndex.keyName -> sourceEdgeIdx,
      DSALinkSinkNodeType.keyName -> sinkNodeType.toString,
      DSALinkSinkNodeId.keyName -> sinkNodeId,
      DSALinkSinkIndex.keyName -> sinkEdgeIdx
    )

  override def toString: String =
    s"$sourceNodeType.$sourceNodeId[$sourceEdgeIdx] --> $sinkNodeType.$sinkNodeId[$sinkEdgeIdx]"
}
