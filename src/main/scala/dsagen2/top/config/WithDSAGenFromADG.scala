package dsagen2.top.config

import chipsalliance.rocketchip.config.{Config, Parameters}
import dsagen2.top.module.DSAGen
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile.BuildRoCC
import play.api.libs.json.{JsValue, Json}

import scala.io.Source

class WithDSAGenFromADG(filename: String = "")
    extends Config((site, here, up) => {
      case BuildRoCC =>
        up(BuildRoCC) ++ Seq((p: Parameters) => {
          // The filename can also from environment variable or command line
          val sourceFile = {
            if (filename == "") {
              // There is no filename from from CDE configuration, search in environment
              val adgFileName: Option[String] = sys.env.get("ADG")
              adgFileName match {
                case Some(fileNameFromEnv) =>
                  Source.fromFile(fileNameFromEnv)
                case None =>
                  println("WARNING: There is not ADG file set, search adg.json under chipyard")
                  Source.fromFile("adg.json")
              }
            } else {
              Source.fromFile(filename)
            }
          }
          // Get the String of Json version ADG
          val jsonStr: String = sourceFile.getLines().mkString
          // Parse as json object
          val jsonVal: JsValue = Json.parse(jsonStr)
          sourceFile.close()
          // Generate DSAGen
          val dsa = LazyModule(DSAGen(jsonVal)(p))
          dsa
        })
      case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
    })
