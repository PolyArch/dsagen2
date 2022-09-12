package dsagen2.comp.impl.proto

object XilinxVU9PFpga extends Enumeration with IsFPGA {
  // Fixed parameter
  override val vendor: String = "Xilinx"

  // For now we just use these five group of resource to do DSE,
  // there are more and finer group like SRL and BRAM36, BRAM18.
  // TODO: we should include finer resource group later
  case class FpgaResource(totalCount: Int) extends super.Val

  // Total resource of vu9p
  val totalLUT:  FpgaResource = FpgaResource(1182240)
  val totalDSP:  FpgaResource = FpgaResource(6840)
  val totalFF:   FpgaResource = FpgaResource(2364480)
  val totalBRAM: FpgaResource = FpgaResource(4320)
  val totalURAM: FpgaResource = FpgaResource(960)
}
