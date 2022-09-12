package dsagen2.comp.config.processing_element

import chipsalliance.rocketchip.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec

class PEMetaCtrlTest extends AnyFlatSpec {
  val para : PEMetaCtrlParameters = PEMetaCtrlParameters(resetReg = false)
  "Meta Control Parameter" should "print CSR Field and Int" in {
    implicit val p : Parameters = new WithDsaOperations

    para.csrFieldBits(num_input = 4, num_output = 1)
      //.foreach(println)
  }
}
