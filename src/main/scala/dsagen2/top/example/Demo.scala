package dsagen2.top.example

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.processing_element._
import dsagen2.comp.config.switch._
import dsagen2.comp.module._
import dsagen2.mem.config._
import dsagen2.mem.module._
import dsagen2.sync.config._
import dsagen2.sync.module._
import dsagen2.top.module._

class Demo()(implicit p: Parameters) extends DSAGen {

  /* ----- Node Creation ----- */

  // Sync Nodes
  val vport_i1 = createIVP(new FullIVPConfig)
  val vport_i2 = createIVP(new FullIVPConfig)
  val vport_o1 = createOVP(new FullOVPConfig)
  val vport_o2 = createOVP(new FullOVPConfig)

  // Compute Nodes
  val sw1 = createCompNode(new DefaultSWConfig)
  val sw2 = createCompNode(new DefaultSWConfig)
  val sw3 = createCompNode(new DefaultSWConfig)
  val sw4 = createCompNode(new DefaultSWConfig)
  val pe = createCompNode(new PE_Simple64_Config)
  
  // Memory Nodes
  val dma0: MemNodeModule = createMemNode(new DefaultDMAConfig)

  /* ----- Node Topology ----- */

  vport_i1 --> sw1
  vport_i1 --> sw2
  vport_i2 --> sw1
  vport_i2 --> sw2
  sw3 --> vport_o1
  sw4 --> vport_o1
  sw3 --> vport_o2
  sw4 --> vport_o2
  
  sw1 <-> sw2
  sw1 <-> sw3
  sw2 <-> sw4
  sw3 <-> sw4
  
  sw1 --> pe
  sw2 --> pe
  sw3 --> pe
  sw4 <-> pe

  dma0 --> vport_i1 
  dma0 --> vport_i2
  vport_o1 --> dma0
  vport_o2 --> dma0

  // Finish Stage
  autoConfigure()
  tlNode := getMemAggTLNode
  override val nPTWPorts: Int = numDMA
}
