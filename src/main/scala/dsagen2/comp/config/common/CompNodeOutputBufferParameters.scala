package dsagen2.comp.config.common

import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json.{JsValue, Json, Writes}

// Parameter Case Class Output Buffer, as FIFO, not-configurable
case class CompNodeOutputBufferParameters(outputBufferDepth: Int = 8, staticOutputBuffer: Boolean = false)
    extends JSONParsableParameters {
  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[CompNodeOutputBufferParameters] {
    def writes(para: CompNodeOutputBufferParameters): JsValue = {
      json.deepMerge(
        Json.obj(fields = "outputBufferDepth" -> para.outputBufferDepth, "staticOutputBuffer" -> staticOutputBuffer)
      )
    }
  }
}

// Parameters Constructor
object CompNodeOutputBufferParameters extends JSONParsableConstructor {
  def apply(json: JsValue): CompNodeOutputBufferParameters = {
    CompNodeOutputBufferParameters(
      outputBufferDepth = (json \ "outputBufferDepth").as[Int],
      staticOutputBuffer = (json \ "staticOutputBuffer").as[Boolean]
    )
  }
}
