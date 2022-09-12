package dsagen2.comp.config.reconf

import chisel3.util.log2Ceil
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.diplomacy.{DSANodeModule, NoNodeModule}

/** Config Port Protocol, for now it is fixed.
  * TODO: make it reconfigurable later
  *
  * @param numNodeType    Number of Node Type per protocol
  * @param numNodePerType Number of max node per node type
  * @param numCfgGroup    Number of configurable group per node
  * @param numCfgPerGroup Number of configuration per group
  * @param configBits     Number of bits per configuration
  */
case class CompReconfProto(
  sourceNode:     DSANodeModule = new NoNodeModule,
  numNodeType:    Int = CONF_NODE_TYPE,
  numNodePerType: Int = CONF_NODE_ID,
  numCfgGroup:    Int = CONF_CFG_GROUP,
  numCfgPerGroup: Int = CONF_CFG_IDX,
  configBits:     Int = CONF_CFG_DATA_BITS) {

  // Calculate the number of bits needed per field
  def nodeTypeBits: Int = log2Ceil(numNodeType)

  def nodeIdBits: Int = log2Ceil(numNodePerType)

  def groupBits: Int = log2Ceil(numCfgGroup)

  def indexBits: Int = log2Ceil(numCfgPerGroup)

  override def equals(obj: Any): Boolean = {
    obj match {
      case proto: CompReconfProto =>
        this.numNodeType == proto.numNodeType &&
          this.numNodePerType == proto.numNodePerType &&
          this.numCfgGroup == proto.numCfgGroup &&
          this.numCfgPerGroup == proto.numCfgPerGroup &&
          this.configBits == proto.configBits
      case _ => false
    }
  }
}
