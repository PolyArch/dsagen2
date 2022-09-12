package dsagen2.comp.bundle

import chisel3._
import dsagen2.comp.config.CompNodeParameters

/** This bundle describe the data structure of Compute Data Field
  * It is just a wrapper of vector of UInt, NOTHING more
  *
  * @param compNode Compute Node Parameters
  */
class CompDataBundle(val compNode: CompNodeParameters) extends Bundle {
  /* ---------- Hardware ---------- */
  // Decomposable: Vector of Tagged Data,
  val vecData: Vec[UInt] = Vec(compNode.numCompUnit, Output(UInt(compNode.compUnitBits.W)))
}
