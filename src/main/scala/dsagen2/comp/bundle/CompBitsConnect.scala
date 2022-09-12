package dsagen2.comp.bundle

import chisel3._

object CompBitsConnect {
  def apply(sink: Data, source: Data): Unit = {
    (sink, source) match {
      case (sinkBits: CompDirBundle, sourceBits: CompDirBundle) =>
        // Valid
        sinkBits.valid match {
          case Some(sinkValid) =>
            sourceBits.valid match {
              case Some(sourceValid) => sinkValid := sourceValid
              case None              => sinkValid := false.B
            }
          case None =>
        }
        // Ready
        sourceBits.ready match {
          case Some(sourceReady) =>
            sinkBits.ready match {
              case Some(sinkReady) => sourceReady := sinkReady
              case None            => sourceReady := false.B
            }
          case None =>
        }
        // Control
        sinkBits.ctrl match {
          case Some(sinkCtrl) =>
            sourceBits.ctrl match {
              case Some(sourceCtrl) => sinkCtrl <> sourceCtrl
              case None             => sinkCtrl := DontCare
            }
          case None =>
        }
        // Data
        sinkBits.data match {
          case Some(sinkData) =>
            sourceBits.data match {
              case Some(sourceData) => CompDataConnect(sinkData, sourceData)
              case None             => sinkData := DontCare
            }
          case None =>
        }
      case _ =>
        require(requirement = false, s"CompBitsConnect only support connection between CompBitsBundle")
    }
  }
}
