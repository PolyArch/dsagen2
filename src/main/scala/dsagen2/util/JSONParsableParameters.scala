package dsagen2.util

import play.api.libs.json.{JsObject, JsValue, Json, Writes}

import scala.language.implicitConversions

trait JSONParsableParameters {
  implicit def covert2JsonParsedPara[T <: JSONParsableParameters](wT: Writes[T])
  : Writes[JSONParsableParameters] = wT.asInstanceOf[Writes[JSONParsableParameters]]

  implicit val paraWrites: Writes[JSONParsableParameters]

  def toJSON: JsValue = Json.toJson(this)

  def emitJSON: String = Json.prettyPrint(toJSON)

  lazy val json: JsObject = Json.obj(
    "parameterClassName" -> Json.toJson(this.getClass.getName),
  )
}
