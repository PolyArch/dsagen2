package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.config.CompKeys.SwitchRouting
import dsagen2.comp.config.switch.SWRoutingParameters

class SWCfgBits(val numInput: Int, val numOutput: Int)(implicit val p: Parameters) extends Bundle {
  // MSB
  val routing: Option[MixedVec[UInt]] = SWCfgBits.OutputRouting(numInput, numOutput)
  // Name it for debug purpose
  routing match {
    case Some(routes) =>
      val numOutSubnet: Int = routes.length
      val numSubnet:    Int = numOutSubnet / numOutput
      for (outIdx <- 0 until numOutput) {
        for (subIdx <- 0 until numSubnet) {
          routes(outIdx * numSubnet + subIdx).suggestName(s"route_Out${outIdx}Sub$subIdx")
        }
      }
    case None =>
  }
  val enabled: Bool = Bool()
  // LSB
}

object SWCfgBits {
  // Output Routing
  def OutputRouting(numInput: Int, numOutput: Int)(implicit p: Parameters): Option[MixedVec[UInt]] = {
    // Get the routing parameter
    val routeParam: Option[SWRoutingParameters] = p.lift(SwitchRouting)
    routeParam match {
      case Some(value) =>
        val bitsNeededForEachSubnet: Seq[Int] = value.csrFieldBits(numInput, numOutput).map(_._2)
        Some(MixedVec(bitsNeededForEachSubnet.map(w => UInt(w.W))))
      case None => None
    }
  }
}
