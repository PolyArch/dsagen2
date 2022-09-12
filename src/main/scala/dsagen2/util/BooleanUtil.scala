package dsagen2.util

import scala.language.implicitConversions

object BooleanUtil {
  implicit def boolean2int(b:Boolean): Int = if (b) 1 else 0
}
