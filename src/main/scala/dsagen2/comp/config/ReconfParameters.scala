package dsagen2.comp.config

import chipsalliance.rocketchip.config.Parameters

trait ReconfParameters {
  def csrFieldBits(num_input: Int, num_output: Int)(implicit p: Parameters): Seq[(String, Int)]
}

case class DummyReconfParameters() extends ReconfParameters {
  override def csrFieldBits(num_input: Int, num_output: Int)(implicit p: Parameters): Seq[(String, Int)] = Seq(("", 0))
}
