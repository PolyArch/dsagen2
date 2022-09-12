package dsagen2.sync.impl

import chisel3._
import chisel3.util.log2Ceil
import dsagen2.comp.config.reconf.CompReconfEdge
import dsagen2.top.bundle.ReconfPort
import dsagen2.top.config.DSAFixedConfig.CONF_CFG_DATA_BITS
import dsagen2.top.config.DebugPrintable
import dsagen2.top.config.enumeration.VPImplMode.{FullXBarVP, VPImpl}
import dsagen2.top.diplomacy.DSANodeModule

trait VPNodeModule extends DSANodeModule {}

abstract class VectorPortImpl extends MultiIOModule with DebugPrintable {

  // Extract parameter
  val reconfParam: Option[CompReconfEdge]
  val vpNodeId:    Int
  val vpImpl:      VPImpl
  val vpNodeTpe:   UInt
  val vecWidth:    Int
  val isIVP:       Boolean

  def supportState: Boolean

  def comp2mem: Int

  def supportPad: Boolean

  def padWidth: Int

  // Derived parameter
  def isOVP: Boolean = !isIVP

  def statedIVP: Boolean = isIVP && supportState

  def statedOVP: Boolean = isOVP && supportState

  // Calculate how many bits need for output vector port compute side output routing
  // 1 bit to turn on the vector port, numQueueInput * log2Ceil(numQueueInput + 1) bits for routing
  def csrBits: Int = 1 + vecWidth * log2Ceil(vecWidth + 1)

  // How many config group needed
  def cfgGroup: Int = csrBits / CONF_CFG_DATA_BITS + {
    if (csrBits % CONF_CFG_DATA_BITS == 0) 0 else 1
  }

  // Extract the reconfiguration port from diplomacy node
  lazy val configPort: Option[ReconfPort] = reconfParam match {
    case Some(param) => Some(IO(Flipped(new ReconfPort(param))))
    case None        => None
  }

  // Bitstream Register for Holding the configuration
  lazy val configStateReg: Option[Vec[UInt]] =
    if (vpImpl == FullXBarVP) Some(RegInit(VecInit(Seq.fill(cfgGroup)(0.U(CONF_CFG_DATA_BITS.W)))))
    else None

  // Configuration and pass down the configuration
  def confVP(): Bool = {
    if (vpImpl == FullXBarVP) {
      // Sanity check
      require(reconfParam.isDefined, s"Full XBar has no reconfiguration port defined")
      // Create config this wire
      val configThis: Bool = configPort.get.valid && configPort.get.nodeId === vpNodeId.U &&
        configPort.get.nodeType === vpNodeTpe
      // Reconfiguration, update the input vector port output xbar routing info
      when(configThis) {
        configStateReg.get.apply(configPort.get.cfgIndex) := configPort.get.cfgBits
      }
      configThis
    } else false.B
  }
}
