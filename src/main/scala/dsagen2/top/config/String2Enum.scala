package dsagen2.top.config

import dsagen2.util.AppUtil.subObjects

object String2Enum {
  // Get all kinds of Enumeration under dsagen2
  val AllEnumerations: Seq[Enumeration] = subObjects[Enumeration]("dsagen2", classOf[Enumeration])

  // Convert the String to the Enumeration Value
  def apply(str: String): Enumeration#Value = {

    // Find the enumeration group that has the enumeration whose name is the same as provided
    val targetGroup = AllEnumerations.find(enumGroup => enumGroup.values.exists(_.toString == str))

    // In the target group, find the enumeration with the same name
    targetGroup match {
      case Some(group) =>
        // Find the enumeration based on its name
        group.values.find(_.toString == str).get.asInstanceOf[Enumeration#Value]
      case None =>
        require(requirement = false, s"In all Enumeration Group ($AllEnumerations), no one contains $str")
        throw new NoSuchElementException
    }
  }
}
