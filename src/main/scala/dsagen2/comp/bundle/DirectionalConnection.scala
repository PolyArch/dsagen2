package dsagen2.comp.bundle

import chipsalliance.rocketchip.config.{Config, Field}

object DirectionalConnection {
  // Downward means the bundle direction is the same as diplomatic bond
  case object HasDownwardConnect extends Field[Boolean](false)

  // Upward means that the bundle direction is the opposite as diplomatic bond
  case object HasUpwardConnect extends Field[Boolean](false)

  class DownwardConnection
      extends Config((site, here, up) => { case HasDownwardConnect =>
        true
      })

  class UpwardConnection
      extends Config((site, here, up) => { case HasUpwardConnect =>
        true
      })
}
