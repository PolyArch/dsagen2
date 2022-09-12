package dsagen2.comp.bundle

import chisel3._
import dsagen2.top.config.DSAFixedConfig.COMP_TYPE_BITS

/** This bundle select a set of signal from compute node that together form the performance event.
  * The performance event will trigger the increment of performance cycle counter
  */
class CompNodeStatus extends Bundle {
  // Whether compute node is alive or dead (it is configured)
  val alive: Bool = Output(Bool())

  // Compute node is being config
  val configured: Bool = Bool()

  // Whether the compute node has pending data to process
  val busy: Bool = Bool()

  // Whether the compute node is active process data: doing operation or moving data
  val fired: Bool = Bool()

  // The type of compute node
  val hwType: UInt = UInt(COMP_TYPE_BITS.W)
}
