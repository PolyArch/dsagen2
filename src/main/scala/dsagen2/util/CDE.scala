package dsagen2.util

import chipsalliance.rocketchip.config.{Field, Parameters}
import dsagen2.comp.config.{DummyReconfParameters, ReconfParameters}
import dsagen2.top.config.{JsonParsableKey, ReconfKey}
import play.api.libs.json._

// CDR Utility
object CDE {
  // Collect Typed (traited) case class in CDE parameter
  def cdeCollectParams[Key, Val](keys : Seq[Key])(implicit p : Parameters) : Seq[Val] = {
    keys.map{ key =>
      try{
        p(key.asInstanceOf[Field[Val]])
      }catch{
        case e : Throwable =>
          println(e)
          require(requirement = false,
            s"key = $key does not point to expected parameters, did you forget to mix parameter?")
          DummyReconfParameters().asInstanceOf[Val]
      }
    }
  }

  /**
   * Collect ReconfParameters from CDE by given keys, convert them to JsObject
   * @param keys interested keys, usually reconfigurable keys
   * @param num_input number of inputs/outputs, maybe input_edges/output_edges parameters, in future
   * @param num_output same as above
   * @param p node parameter
   * @tparam Key_t Key type
   * @return (Seq[FieldName, Seq(FieldHighBit, FieldLowBit), FieldIsDefined], Node Parameter)
   */
  def cde2ReconfPara2BitStreamEnc[Key_t](keys : Seq[Key_t], num_input : Int, num_output:Int)(
    implicit p : Parameters) : (Seq[(String, Seq[Int], Boolean)], Parameters) = {
    val reconfParas : Seq[ReconfParameters] = cdeCollectParams(keys)
    val encoding : Seq[(String, Seq[Int], Boolean)]=
      reconfParas.flatMap(_.csrFieldBits(num_input,num_output))
        .scanLeft(("Enabled", Seq(0, 0), true )){ // TODO: enabled is always on for now
          case ((_, high :: low :: Nil, _), (fieldName, fieldBits)) =>
            require(high >= low && low >= 0, s"previous location[$high, $low] is problematic")
            //println(s"field = $fieldName, bits = $fieldBits, high = $high, low = $low")
            if(fieldBits == 0){
              (fieldName, Seq(high, low), false) // Field not defined, just pass through
            }else if(fieldBits > 0){
              (fieldName, Seq(high + fieldBits, high + 1), true) // field defined, add its own field bits
            }else{  // error
              require(requirement = false, s"How can possibly a field use negative #bits")
              (fieldName, Seq(-1, -1), false)
            }

        }
    (encoding, p) // enabled or not has nothing to do with reconfigurable
  }

  // Collect ReconfParameters from CDE by given keys, convert them to JsObject
  def cdeReconfPara2JsObject[Key_t](keys : Seq[Key_t],
                                    num_input : Int,
                                    num_output:Int)(implicit p : Parameters) : JsObject = {
    val (field2HighLow, _) = cde2ReconfPara2BitStreamEnc(keys, num_input, num_output)
    JsObject(
      field2HighLow
        .map(x=>x._1 -> Json.toJson(x._2))
    )
  }

  /**
   * This function will convert the two set of key to the json object.
   * For memory node that does not have reconfigurable parameters, it will only print the non-runtime reconfigurable
   * parameters
   *
   * @param paraKey Parameter Key points to the parameter case class that is not reconfigurable, like output buffer depth
   * @param reconfKey Reconfigurable Key points to the parameters that can be runtime reconfigured
   * @param nI Number of Input of the node
   * @param nO Number of Output of the node
   * @param p CDE Parameter
   * @tparam PKey This should be [[JsonParsableKey]]
   * @tparam RKey This should be [[ReconfKey]]
   * @return
   */
  def cde2FullJson[PKey, RKey](paraKey : Seq[PKey],
                               reconfKey : Seq[RKey] = Nil,
                               nI:Int, nO:Int)(implicit p : Parameters) : JsObject = {
    val paraJson : JsObject = cde2json(paraKey)
    val bitstreamJson : Option[JsObject] = reconfKey match {
      case Nil => None
      case keys : Seq[RKey] => Some(cdeReconfPara2JsObject(keys, nI, nO))
    }
    bitstreamJson match {
      case Some(value) => paraJson deepMerge Json.obj(fields = "ConfigBitEncode" -> value)
      case None => paraJson
    }
  }


  // Convert CDE parameter to JsValue by given Keys
  def cde2json[K](keys : Seq[K])(implicit p : Parameters) : JsObject = {
    // Convert all JSON Parsable Parameter to JSObject, if key is not defined in CDE's p, JsNull will
    // be return and filtered out when converted into json object
    val key2jsonParam : Seq[(String, JsValue)] = keys.map { key =>
      val json = try{
        val para = p(key.asInstanceOf[Field[JSONParsableParameters]])
        val paraJson = para.toJSON
        paraJson
      }catch{
        case e : Throwable =>
          require(requirement = false,
            s"Error thown when key $key try to access its parameter, " +
              s"error message : ${e.toString}")
          JsNull
      }
      // Please use getName if you want to find the case class that is used as key
      key.getClass.getName -> json
    }
    JsObject(key2jsonParam.filter(_._2 != JsNull))
  }

  // Convert JSON string to CDE parameter
  import scala.reflect.runtime.universe
  val runtimeMirror: universe.Mirror = universe.runtimeMirror(getClass.getClassLoader)
  def json2cde(jsonVal: JsValue) : Parameters = {
    val json : JsObject = jsonVal.as[JsObject]
    val jsonKey2Value : Seq[(String, JsValue)] = json.fields.filter{
      case (_, value) => value \ "parameterClassName" match {
        case JsDefined(_) => true
        case _: JsUndefined =>false
      }
    }
    val paraMap : Map[Any, Any] = jsonKey2Value.map{
      case (keyName, value) =>
        val jsonPara : JsObject = value.as[JsObject]
        val paraClassName : String = (jsonPara \ "parameterClassName").as[String]
        val moduleKey = runtimeMirror.staticModule(keyName)
        val modulePara = runtimeMirror.staticModule(paraClassName)
        // Get CDE Key
        val objKey = runtimeMirror.reflectModule(moduleKey).instance
        // Get CDE Parameters
        val objPara : JSONParsableParameters = runtimeMirror.reflectModule(modulePara).instance
          .asInstanceOf[JSONParsableConstructor].apply(jsonPara)
        // Modify the CDE Parameter
        objKey -> objPara
    }.toMap
    // Alter the parameter by applying the map, TODO: use alterPartial instead on alterMap
    Parameters.empty.alterMap(paraMap)
  }
}
