package dsagen2.mem.module.stab

import dsagen2.mem.bundle.{MemReadBundle, MemWriteBundle}
import dsagen2.mem.config.MemNodeParameters

trait BaseStrTable {
  val memNode:       MemNodeParameters
  val memReadPorts:  Seq[MemReadBundle]
  val memWritePorts: Seq[MemWriteBundle]
}
