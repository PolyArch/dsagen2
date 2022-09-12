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

// Mesh DSA Example
class DSAGemm(implicit p: Parameters) extends DSAGen {
  /* ----- Node Creation ----- */

  // Sync Nodes
  val ivps: Array[IVPNodeModule] = Array.fill(4)(createIVP(new FullIVPConfig))
  val ovps: Array[OVPNodeModule] = Array.fill(2)(createOVP(new FullOVPConfig))

  // Compute Nodes
  val meshSize: Int = 5
  val swMesh:   Array[Array[CompNodeModule]] = createCompNodeMesh(new DefaultSWConfig, meshSize, meshSize)
  val peMesh:   Array[Array[CompNodeModule]] = createCompNodeMesh(new PE_Full64_SIMD_Config, meshSize - 1, meshSize - 1)

  // Memory Nodes
  val dma0: MemNodeModule = createMemNode(new DefaultDMAConfig)
  val spm0: MemNodeModule = createMemNode(new DefaultSPMConfig)
  val gen0: MemNodeModule = createMemNode(new DefaultGENConfig)
  val reg0: MemNodeModule = createMemNode(new DefaultREGConfig)
  val rec0: MemNodeModule = createMemNode(new DefaultRECConfig)

  /* ----- Node Topology ----- */

  // Building Compute System with certain topology, flatten switch network
  Switch4_PE1_MeshTopology(swMesh, peMesh)
  val swList: List[CompNodeModule] = swMesh.flatten.toList

  // Input Vector Ports, random connection to switch network
  ivps.zip(swList.grouped(meshSize).toSeq).foreach { case (ivp, sws) => ivp ==> sws }

  // Output Vector Ports
  ovps.zip(swList.reverse.grouped(meshSize).toSeq).foreach { case (ovp, sws) => ovp <== sws }

  // DMA
  ivps.foreach(dma0 --> _);
  ovps.foreach(dma0 <-- _)

  // SPM
  ivps.foreach(spm0 --> _);
  ovps.foreach(spm0 <-- _)

  // GEN
  ivps.foreach(gen0 --> _);
  ovps.foreach(gen0 <-- _)

  // REC
  ivps.foreach(rec0 --> _);
  ovps.foreach(rec0 <-- _)

  // REG
  ovps.foreach(reg0 <-- _)

  // Finish Stage
  autoConfigure()
  tlNode := getMemAggTLNode
  override val nPTWPorts: Int = numDMA
}
