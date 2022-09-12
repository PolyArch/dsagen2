package dsagen2.misc

import org.scalatest.flatspec.AnyFlatSpec

class UIntUtilUtilTest extends AnyFlatSpec{
  "The reduceLeft function used by Optional Cat" should "works" in {
    def Cat(opUInts : Option[String] * ) : Option[String] = {
      def reduceFunc(left : Option[String], right : Option[String]) : Option[String] = {
        (left, right) match {
          case (Some(l), Some(r)) => Some(l + r)
          case (None, Some(r)) => Some(r)
          case (Some(l), None) => Some(l)
          case (None, None) => None
        }
      }
      opUInts.reduceLeft(reduceFunc)
    }
    require(Cat(Some("1"), None, Some("3")).get == "13", s"It does not work")
  }

}
