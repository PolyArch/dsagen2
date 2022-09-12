package dsagen2.util

import scala.language.implicitConversions

object IntUtil {
  /**
   * Convert unsigned Int to signed
   * @param uint unsigned int
   * @param width width of value
   * @return signed int
   */
  def uint2sint(uint : Int, width : Int) : Int = {
    require(uint >= 0, s"Unsigned Int ($uint) is negative")
    val largest_sint : Int = (1 << (width - 1)) - 1
    val smallest_sint : Int = - (1 << (width - 1))
    if(uint <= largest_sint){
      uint
    }else{
      val diff : Int = uint - largest_sint
      smallest_sint + diff - 1
    }
  }

  /**
   * Convert integer to boolean
   * @param i integer
   * @return Boolean
   */
  implicit def int2boolean(i: Int): Boolean = if (i == 0) false else true


  def leftRotate(n: Int, d: Int): Int = (n << d) | (n >> (32 - d))
  def rightRotate(n: Int, d: Int): Int = (n >> d) | (n << (32 - d))
}
