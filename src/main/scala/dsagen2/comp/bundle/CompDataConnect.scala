package dsagen2.comp.bundle

import dsagen2.util.StreamUtil.vecDataConnect

object CompDataConnect {
  // Connect Two CompDataBundle
  def apply(sinkDataBundle: CompDataBundle, sourceDataBundle: CompDataBundle): Unit =
    // Connect
    vecDataConnect(sinkDataBundle.vecData, sourceDataBundle.vecData)
}
