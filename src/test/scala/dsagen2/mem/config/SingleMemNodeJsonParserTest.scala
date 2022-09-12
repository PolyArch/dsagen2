package dsagen2.mem.config

import dsagen2.top.config.DSAFixedConfig.MAX_LOCAL_VPORT_ID_BIT
import dsagen2.top.config.operation.Operation.FloatAdd
import dsagen2.top.diplomacy.DSANodeType.DirectMemoryAccess
import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json.Json

class SingleMemNodeJsonParserTest extends AnyFlatSpec{

  val memNode : MemNodeParameters = MemNodeParameters(
    nodeId = 0,
    nodeType = DirectMemoryAccess,
    AtomicOperations = Set(FloatAdd),
    numGenDataType = 0
  )
  "A Memory Node parameter" should "be printed as JSON" in {
    val jsonStr : String = memNode.emitJSON
    //println("Memory Node emitted JSON : \n" + jsonStr)
  }

  "Memory Node parsed by JSON" should "be identical with the original one" in {
    val memNodeByJson : MemNodeParameters = MemNodeParameters(Json.parse(memNode.emitJSON))
    require(memNodeByJson == memNode, s"Pass through JSON parser make parameters different")
  }

  "Maximum Local Port Bits" should " = 7" in {
    require(MAX_LOCAL_VPORT_ID_BIT == 7, s"Maximum Local Port Bits should be 7, but it is $MAX_LOCAL_VPORT_ID_BIT")
  }
}
