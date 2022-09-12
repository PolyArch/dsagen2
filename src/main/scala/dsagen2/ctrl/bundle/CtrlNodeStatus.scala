package dsagen2.ctrl.bundle

import chisel3._

/** This bundle record the control node (stream dispatcher)
  */
class CtrlNodeStatus extends Bundle {

  // Stream dispatcher is in reloading bitstream mode
  val reloadBits: Bool = Bool()

  // Stream dispatcher is configuring bits to all nodes (loading bits does not mean configuring)
  val configuringBits: Bool = Bool()

  // Stream dispatcher is in idle mode to wait the last config bits to reach its destination
  val loadLastBits: Bool = Bool()

  // Stream Dispatcher stream entry queue status
  val noValidStrEntry:   Bool = Bool()
  val hasValidStrEntry:  Bool = Bool() // Not empty, not full
  val fullValidStrEntry: Bool = Bool()

  // Stream Dispatcher sync entry queue status
  val noValidSyncEntry:   Bool = Bool()
  val hasValidSyncEntry:  Bool = Bool()
  val fullValidSyncEntry: Bool = Bool()
}
