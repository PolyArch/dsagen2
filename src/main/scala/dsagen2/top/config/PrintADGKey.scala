package dsagen2.top.config

import chipsalliance.rocketchip.config.{Config, Field}

case object PrintADGKey extends Field[(String, String)](("./", ""))

class PrintADG(targetDir: String = "./adg", fileName: String = "")
    extends Config((site, here, up) => {
      case PrintADGKey => {
        // Get ADG Directory
        val adgDir = sys.env.get("ADG_DIR") match {
          case Some(value) => value
          case None =>
            sys.env.get("DSAGEN_DIR") match {
              case Some(dsaDir) => dsaDir + "/adg"
              case None =>
                require(requirement = false, s"Please set environment ADG_DIR or DSAGEN_DIR for ADG output")
                targetDir
            }
        }
        // Get the ADG Filename
        val adgName = sys.env.get("ADG") match {
          case Some(value) => value
          case None        => fileName
        }
        // Return Directory and Name
        (adgDir, adgName)
      }
    })
