package dsagen2.sync.bundle

import chisel3._

/** This bundle select a set of signal from synchronization node that together forms the performance event of sync node
  * The performance event will trigger the increment of corresponding performance cycle counter
  */
class VectorPortStatus extends Bundle {

  // Whether this vector port is alive: taken by some memory engine or has data inside it
  val alive: Bool = Bool()

  // Whether this vector port is being configured
  val configured: Bool = Bool()

  // Whether vector port contains data
  val hasData: Bool = Bool()

  // The compute side fired
  val compFired: Bool = Bool()

  // The memory side fired
  val memFired: Bool = Bool()
}
