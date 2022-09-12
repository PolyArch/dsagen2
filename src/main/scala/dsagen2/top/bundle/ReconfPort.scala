package dsagen2.top.bundle

import chisel3._
import dsagen2.comp.config.reconf.CompReconfEdge

class ReconfPort(val protocol: CompReconfEdge) extends Bundle {
  // 1 bit, valid
  val valid: Bool = Output(Bool())
  // 1 bit, reset
  val reset: Bool = Output(Bool())
  // 2 bit, 4 kinds of node
  val nodeType: UInt = Output(UInt(protocol.nodeTypeBits.W))
  // 8 bit, up to 256 nodes per node type
  val nodeId: UInt = Output(UInt(protocol.nodeIdBits.W))
  // 2 bit, 4 different configure group
  val cfgGroup: UInt = Output(UInt(protocol.groupBits.W))
  // 4 bit, configuration position in that group
  val cfgIndex: UInt = Output(UInt(protocol.indexBits.W))
  // 48 bit, actual configuration data
  val cfgBits: UInt = Output(UInt(protocol.configBits.W))
}
