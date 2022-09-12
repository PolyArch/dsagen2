package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Parameters
import dsagen2.comp.config.CompKeys._
import dsagen2.comp.config.ReconfParameters
import dsagen2.comp.config.switch.DefaultSWConfig
import dsagen2.util.CDE.cdeCollectParams
import org.scalatest.flatspec.AnyFlatSpec

class PEReconfParamTest extends AnyFlatSpec {
  "Reconfigurable Parameters" should "be collected" in {
    implicit val p : Parameters = Parameters.empty
    val pe : Parameters = new DefaultPEConfig
    val sw : Parameters = new DefaultSWConfig

    val deepDelayFIFOPE = pe.alterPartial({
      case DsaOperations => pe(DsaOperations).copy(maxFifoDepth = 64)
    })

    val peParas: Seq[ReconfParameters] =
      cdeCollectParams(PEKeys.reconfKeys)(deepDelayFIFOPE)
    val swParas: Seq[ReconfParameters] =
      cdeCollectParams(SWKeys.reconfKeys)(sw)

    peParas.foreach(para=>para.csrFieldBits(num_input = 4, num_output = 5)(deepDelayFIFOPE)
      //.foreach(println)
    )
    swParas.foreach(para=>para.csrFieldBits(num_input = 4, num_output = 5)(sw)
      //.foreach(println)
    )

  }
}
