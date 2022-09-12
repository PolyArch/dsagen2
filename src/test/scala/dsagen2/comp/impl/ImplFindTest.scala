package dsagen2.comp.impl

import dsagen2.util.AppUtil.subObjects
import org.scalatest.flatspec.AnyFlatSpec

class ImplFindTest extends AnyFlatSpec{
  "Implementation Find" should "return all object inherit IsFunctionUnitImplementation" in {
    // Get all object of IsFunctionUnitImplementation
    val implObjects : Seq[IsFunctionUnitImplementation] =
      subObjects[IsFunctionUnitImplementation](classOf[IsFunctionUnitImplementation].getPackage.getName,
        classOf[IsFunctionUnitImplementation])

    require(implObjects.nonEmpty, s"Cannot find FU implementation")

    // Print the name of object
    // println(s"${implObjects.size} kind(s) of FU implementation found")
    // implObjects.foreach(o=>println(o.getClass.getName))
  }
}
