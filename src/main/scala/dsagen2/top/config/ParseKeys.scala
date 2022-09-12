package dsagen2.top.config

object ParseKeys extends Enumeration {
  case class DSAGenParseKey(keyName: String) extends super.Val

  val DSAGenNodes: DSAGenParseKey = DSAGenParseKey("DSAGenNodes")
  val DSAGenEdges: DSAGenParseKey = DSAGenParseKey("DSAGenEdges")
  val CompNodes:   DSAGenParseKey = DSAGenParseKey("ComputeNodes")
  val CompEdges:   DSAGenParseKey = DSAGenParseKey("ComputeEdges")
  val SyncNodes:   DSAGenParseKey = DSAGenParseKey("SyncNodes")
  val SyncEdges:   DSAGenParseKey = DSAGenParseKey("SyncEdges")
  val MemNodes:    DSAGenParseKey = DSAGenParseKey("MemNodes")
  val MemEdges:    DSAGenParseKey = DSAGenParseKey("MemEdges")

  val DSALinkSourceNodeType: DSAGenParseKey = DSAGenParseKey("SourceNodeType")
  val DSALinkSinkNodeType:   DSAGenParseKey = DSAGenParseKey("SinkNodeType")
  val DSALinkSourceNodeId:   DSAGenParseKey = DSAGenParseKey("SourceNodeId")
  val DSALinkSinkNodeId:     DSAGenParseKey = DSAGenParseKey("SinkNodeId")
  val DSALinkSourceIndex:    DSAGenParseKey = DSAGenParseKey("SourceIndex")
  val DSALinkSinkIndex:      DSAGenParseKey = DSAGenParseKey("SinkIndex")
}
