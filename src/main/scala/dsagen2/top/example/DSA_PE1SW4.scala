package dsagen2.top.example

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.processing_element._
import dsagen2.comp.config.switch.DefaultSWConfig
import dsagen2.comp.module.CompNodeModule
import dsagen2.mem.config.DefaultDMAConfig
import dsagen2.mem.module.MemNodeModule
import dsagen2.sync.config.{SimpleIVPConfig, SimpleOVPConfig}
import dsagen2.sync.module.{IVPNodeModule, OVPNodeModule}
import dsagen2.top.module.DSAGen

class DSA_PE1SW4(implicit p: Parameters) extends DSAGen {

  // Sync Node definition
  val ivp0: IVPNodeModule = createIVP(new SimpleIVPConfig)
  val ivp1: IVPNodeModule = createIVP(new SimpleIVPConfig)

  val ovp0: OVPNodeModule = createOVP(new SimpleOVPConfig)
  val ovp1: OVPNodeModule = createOVP(new SimpleOVPConfig)

  // Compute Node definition
  val pe1: CompNodeModule = createCompNode(new PEAddMulConfig)
  val sw0: CompNodeModule = createCompNode(new DefaultSWConfig)
  val sw1: CompNodeModule = createCompNode(new DefaultSWConfig)
  val sw2: CompNodeModule = createCompNode(new DefaultSWConfig)
  val sw3: CompNodeModule = createCompNode(new DefaultSWConfig)

  // Memory Node definition
  val dma0: MemNodeModule = createMemNode(new DefaultDMAConfig)

  /* ----- Compute System Connection ----- */
  // Routing Network
  sw0 <-> sw1
  sw0 <-> sw2
  sw1 <-> sw3
  sw2 <-> sw3
  // Switch to PE1
  sw0 --> pe1
  sw1 --> pe1
  sw2 --> pe1
  sw3 <-> pe1

  // Synchronization system connection
  ivp0 --> sw0
  ivp0 --> sw1
  ivp1 --> sw0
  ivp1 --> sw1

  sw2 --> ovp0
  sw3 --> ovp0
  sw2 --> ovp1
  sw3 --> ovp1

  // Memory system connection
  dma0 --> ivp0
  dma0 --> ivp1
  ovp0 --> dma0
  ovp1 --> dma0

  autoConfigure()
  tlNode := getMemAggTLNode
  override val nPTWPorts: Int = numDMA
}
