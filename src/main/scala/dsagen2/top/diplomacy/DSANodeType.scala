package dsagen2.top.diplomacy

/* --------- Node abbreviation ---------- */
object DSANodeType extends Enumeration {
  type DSAGenNodeType = Value

  // Comp System (Computing System), 2 nodes (Abbreviation: PE, SW)
  val ProcessingElement: DSAGenNodeType = Value("ProcessingElement")
  val Switch:            DSAGenNodeType = Value("Switch")

  // Synchronization System, 2 nodes (Abbreviation: IVP, OVP )
  val InputVectorPort:  DSAGenNodeType = Value("InputVectorPort")
  val OutputVectorPort: DSAGenNodeType = Value("OutputVectorPort")

  // Memory System, 6 nodes (Abbreviation: DMA, SPM, REC, DIS, GEN, REG)
  val DirectMemoryAccess: DSAGenNodeType = Value("DirectMemoryAccess")
  val ScratchpadMemory:   DSAGenNodeType = Value("ScratchpadMemory")
  val RecurrenceEngine:   DSAGenNodeType = Value("RecurrenceEngine")
  val DiscardEngine:      DSAGenNodeType = Value("DiscardEngine")
  val GenerateEngine:     DSAGenNodeType = Value("GenerateEngine")
  val RegisterEngine:     DSAGenNodeType = Value("RegisterEngine")

  // Control System, 1 nodes (Abbreviation: DISP)
  val StreamDispatcher: DSAGenNodeType = Value("StreamDispatcher")

  // Enumeration Type of the DSANode Conversion

  import scala.language.implicitConversions

  implicit def NodeTypeToString(x: DSAGenNodeType): String = x.toString

  implicit def String2DSANodeType(x: String): DSAGenNodeType =
    values.find { enum => enum.toString == x } match {
      case Some(found) => found
      case None =>
        require(requirement = false, s"There is no DSANodeType whose name is $x")
        ProcessingElement
    }

  // Check if it is memory node type
  def isMemNodeType(nodeType: DSAGenNodeType): Boolean = {
    nodeType == DirectMemoryAccess || nodeType == ScratchpadMemory || nodeType == RecurrenceEngine ||
    nodeType == DiscardEngine || nodeType == GenerateEngine || nodeType == RegisterEngine
  }

  // Check if it is compute node type
  def isCompNodeType(nodeType: DSAGenNodeType): Boolean =
    nodeType == ProcessingElement || nodeType == Switch

  // Convert Node Name ("nodeType_nodeId") to (nodeType, nodeId)
  def nodeName2pair(name: String): (DSAGenNodeType, Int) = {
    val split = name.split('.')
    require(split.length == 2, s"$name is not a legal node name")
    val nodeType: DSAGenNodeType = String2DSANodeType(split.head)
    val nodeId:   Int = split(1).toInt
    require(nodeId >= 0, s"Node Id of $nodeType is $nodeId, cannot be negative")
    (nodeType, nodeId)
  }
}
