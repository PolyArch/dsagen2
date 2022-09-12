package dsagen2.top.example

import chipsalliance.rocketchip.config.Parameters
// Import Configuration
import dsagen2.comp.config.processing_element._
import dsagen2.comp.config.switch._
import dsagen2.mem.config._
import dsagen2.sync.config._
// Import Modules
import dsagen2.comp.module._
import dsagen2.mem.module._
import dsagen2.sync.module._
import dsagen2.top.module._

// Developing DSA Example
class DSAGenDev(implicit p: Parameters) extends DSAGen {

  /* ----- Node Creation ----- */

  // Sync Nodes
  val ivp0: IVPNodeModule = createIVP(new SimpleIVPConfig)
  val ivp1: IVPNodeModule = createIVP(new SimpleIVPConfig)
  val ovp0: OVPNodeModule = createOVP(new SimpleOVPConfig)
  val ovp1: OVPNodeModule = createOVP(new SimpleOVPConfig)

  // Compute Nodes
  val pe1: CompNodeModule = createCompNode(new PEAddMulMaxMinConfig)
  val pe2: CompNodeModule = createCompNode(new PEAddMulMaxMinConfig)
  val sw0: CompNodeModule = createCompNode(new DefaultSWConfig)
  val sw1: CompNodeModule = createCompNode(new DefaultSWConfig)
  val sw2: CompNodeModule = createCompNode(new DefaultSWConfig)
  val sw3: CompNodeModule = createCompNode(new DefaultSWConfig)
  val sw4: CompNodeModule = createCompNode(new DefaultSWConfig)

  // Memory Nodes
  val dma0: MemNodeModule = createMemNode(new DefaultDMAConfig)
  val spm0: MemNodeModule = createMemNode(new DefaultSPMConfig)
  val gen0: MemNodeModule = createMemNode(new DefaultGENConfig)
  val reg0: MemNodeModule = createMemNode(new DefaultREGConfig)
  val rec0: MemNodeModule = createMemNode(new DefaultRECConfig)

  /* ----- Node Connection ----- */
  // Routing Network
  sw0 <-> sw1
  sw0 <-> sw2
  sw1 <-> sw3
  sw2 <-> sw3
  sw1 <-> sw4
  // Connection to PE1
  sw0 --> pe1
  sw1 --> pe1
  sw2 --> pe1
  sw3 <-> pe1
  // Connection to PE2
  sw1 --> pe2
  sw4 --> pe2
  pe2 <-> sw3

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

  // DMA
  dma0 --> ivp0
  dma0 --> ivp1
  ovp0 --> dma0
  ovp1 --> dma0

  // SPM
  spm0 --> ivp0
  spm0 --> ivp1
  ovp0 --> spm0
  ovp1 --> spm0

  // GEN
  gen0 --> ivp0
  gen0 --> ivp1
  ovp0 --> gen0
  ovp1 --> gen0

  // REG
  ovp0 --> reg0
  ovp1 --> reg0

  // REC
  ovp0 --> rec0
  ovp1 --> rec0
  rec0 --> ivp0
  rec0 --> ivp1

  autoConfigure()
  tlNode := getMemAggTLNode
  override val nPTWPorts: Int = numDMA
}
