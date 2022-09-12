package dsagen2.util

import chisel3.experimental.{BaseModule, ChiselAnnotation, annotate}

object Annotator {
  def vivadoDontTouch(foo: BaseModule) ={
    annotate(
      new ChiselAnnotation {
        override def toFirrtl = new firrtl.AttributeAnnotation(
          foo.toTarget,
          "dont_touch = \"yes\""
        )
      }
    )
    foo
  }
}
