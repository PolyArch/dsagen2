package dsagen2.util

import scala.collection.Set

object EnumerationUtil {
  // Get the Enumeration Object from the provided enumerations, cannot use outerEnum since it is private
  def getEnumGroups(enums : Set[Enumeration#Value]) : Set[Enumeration] = {
    // Select the field that we want to access by String
    val field = classOf[Enumeration#Value].getDeclaredField("scala$Enumeration$$outerEnum")
    // make it accessible
    field.setAccessible(true)
    // for all enumerations, get this field
    enums.map(field.get(_).asInstanceOf[Enumeration])
  }

  // Group the set of enumeration by the enumeration type
  def groupByEnumeration(enums : Set[Enumeration#Value]) : Seq[(Set[Enumeration#Value], Enumeration)] = {
    // Get the enumeration group first
    val enumGroups : Set[Enumeration] = getEnumGroups(enums)
    // For each of the enumeration group, filter out the enumeration belong to this
    enumGroups.map{group =>
      val targetEnums : Set[Enumeration#Value] = enums.filter{enu =>
        val thisGroup : Enumeration = getEnumGroups(Set(enu)).head
        thisGroup == group
      }
      (targetEnums, group)
    }.toSeq
  }
}
