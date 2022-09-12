package dsagen2.comp.config.reconf

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}

case class CompReconfEdge(
  pd:         CompReconfProto = CompReconfProto(),
  pu:         CompReconfProto = CompReconfProto(),
  p:          Parameters = Parameters.empty,
  sourceInfo: SourceInfo = UnlocatableSourceInfo) {
  // TODO: I don't know how we are going to deal with the reconfiguration protocol for now
  //  to me the downward and upward should have same protocol
  require(
    pd == pu,
    s"Downward configuration protocol $pd, " +
      s"is different from the upward $pu"
  )

  def nodeTypeBits: Int = pd.nodeTypeBits.max(pu.nodeTypeBits)

  def nodeIdBits: Int = pd.nodeIdBits.max(pu.nodeIdBits)

  def groupBits: Int = pd.groupBits.max(pu.groupBits)

  def indexBits: Int = pd.indexBits.max(pu.indexBits)

  def configBits: Int = pd.configBits.max(pu.configBits)
}
