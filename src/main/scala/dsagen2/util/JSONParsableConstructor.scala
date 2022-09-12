package dsagen2.util

import play.api.libs.json.JsValue

trait JSONParsableConstructor {
  def apply(json: JsValue): JSONParsableParameters
}
