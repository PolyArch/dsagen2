package dsagen2.comp.impl.proto

import dsagen2.comp.impl.IsFunctionUnitImplementation

// Try to capture the ASIC implementation overhead at the stage of hardware generation

/** An future interface for ASIC fast prediction module in Hardware Generator
  *
  * @param techNode e.g. 28nm
  * @param edkName  e.g. umc28nlp.....
  * @param impl     e.g. implementation
  */
case class ASICParameter(techNode: Int, edkName: String, impl: IsFunctionUnitImplementation) {
  def area: Int = ???

  def power: Int = ???

  def freq: Int = ???
}
