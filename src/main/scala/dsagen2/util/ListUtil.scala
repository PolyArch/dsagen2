package dsagen2.util

import scala.language.implicitConversions

object ListUtil {
  class EnhancedList(ls : List[String]){
    // get list
    def ls() : List[String] = ls
    // Cross Product
    def ^ (that : EnhancedList) : Set[String] = {
      (for(source <- ls; sink <- that.ls()) yield
        source + "_" + sink).toSet
    }
  }

  implicit def set2Enhanced(ls : Set[String]) : EnhancedList = {
    new EnhancedList(ls.toList)
  }
}
