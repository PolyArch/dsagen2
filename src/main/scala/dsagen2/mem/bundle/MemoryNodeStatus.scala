package dsagen2.mem.bundle

import chisel3._
import chisel3.util.Cat
import dsagen2.top.config.DSAFixedConfig.MEM_TYPE_BITS

/** This bundle select a set of signal from memory node that together forms the performance event of memory node
  * The performance event will trigger the increment of corresponding performance cycle counter
  */
class MemoryNodeStatus extends Bundle {

  // Memory engine contains active stream entry in its stream table
  val alive: Bool = Bool()

  // Memory Engine receive stream instantiation request from stream dispatcher
  val newStr: Bool = Bool()

  // Address Generate Unit is producing address from stream table
  val aguReq: Bool = Bool()

  // Stop by read side of memory engine (readPause)
  val readPause: Bool = Bool()

  // Stop by write side of memory engine (writePause)
  val writePause: Bool = Bool()

  // Stop by other reason, like indirect scratchpad conflict causes bank queue full
  val portPause: Bool = Bool()

  // The type of memory engine
  val memType: UInt = UInt(MEM_TYPE_BITS.W)

  // The memory operation that this memory node currently doing
  val doingAtomOp5: Bool = Bool()
  val doingAtomOp4: Bool = Bool()
  val doingAtomOp3: Bool = Bool()
  val doingAtomOp2: Bool = Bool()
  val doingAtomOp1: Bool = Bool()
  val doingAtomOp0: Bool = Bool()
  val doingWrite:   Bool = Bool()
  val doingRead:    Bool = Bool()

  def memOpMask: UInt =
    Cat(doingAtomOp5, doingAtomOp4, doingAtomOp3, doingAtomOp2, doingAtomOp1, doingAtomOp0, doingWrite, doingRead)
}
