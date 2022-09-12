package dsagen2.top.config.enumeration

import scala.language.implicitConversions

/** The implementation of vector port
  */
object VPImplMode extends Enumeration {
  type VPImpl = Value

  implicit def vpImpl2int(impl: VPImpl): Int = impl.id

  implicit def int2vpImpl(int: Int): VPImpl = {
    this.values.filter(_.id == int).toList match {
      case Nil =>
        require(requirement = false, s"Vector Port encoding $int does not map to any implementation")
        NonXBarVP
      case ::(head, tl) =>
        require(tl.isEmpty, s"Vector port implementation encoding $int maps to more than one implementation")
        head
    }
  }

  val FullXBarVP:  VPImpl = Value("FVP") // FVP: Full XBar Vector Port
  val LimitXBarVP: VPImpl = Value("LVP") // LVP: Limited XBar Vector Port
  val NonXBarVP:   VPImpl = Value("NVP") // NVP: Non XBar Vector Port
}
