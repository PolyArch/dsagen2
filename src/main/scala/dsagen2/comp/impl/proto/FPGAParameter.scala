package dsagen2.comp.impl.proto

import dsagen2.comp.impl.IsFunctionUnitImplementation

/** * An future interface for FPGA fast resource module
  *
  * @param fpga e.g. Xilinx VU0P
  * @param impl function unit implementation
  */
case class FPGAParameter(fpga: IsFPGA, impl: IsFunctionUnitImplementation) {
  def numLUT: Int = ???

  def numFF: Int = ???

  def numBRAM: Int = ???

  def numDSP: Int = ???
}
