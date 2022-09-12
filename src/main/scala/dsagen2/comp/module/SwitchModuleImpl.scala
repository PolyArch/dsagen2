package dsagen2.comp.module

import Chisel.DecoupledIO
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import dsagen2.comp.bundle._
import dsagen2.comp.config.CompKeys.{CompNode, SwitchRouting}
import dsagen2.comp.config.reconf.CompReconfEdge
import dsagen2.comp.config.switch.{SWRoutingParameters, WithRouting}
import dsagen2.comp.config.{CompNodeParameters, WithCompNode}
import dsagen2.comp.diplomacy.CompDirEdgeParameters
import dsagen2.comp.impl.{NBufferImpl, RoutingModuleImpl}
import dsagen2.top.config.DSAFixedConfig._
import dsagen2.top.diplomacy.DSANodeType.Switch
import logger.{LogLevel, LogLevelAnnotation}

import scala.collection.mutable.ListBuffer
import scala.math._

class SwitchModuleImpl(
  val compNode:    CompNodeParameters,
  val reconfParam: CompReconfEdge,
  val inParams:    Seq[CompDirEdgeParameters],
  val outParams:   Seq[CompDirEdgeParameters]
)(
  implicit val p: Parameters)
    extends CompModuleImpl
    with RoutingModuleImpl // for each output, get input routed
    with NBufferImpl[CompDataBundle] { // for each output, buffer for output

  /* ------------------------- Extract Parameters           ------------------------- */

  // Routing Parameter
  val routeParam: SWRoutingParameters = p(SwitchRouting)

  /* ------------------------- Parameters Setup             ------------------------- */

  // Setup routing component
  val routeNumInput:  Int = numInput
  val routeNumOutput: Int = numOutput
  val routeCfgGroup :: Nil = Enum(1)

  /* ------------------------- Diplomatic Node              ------------------------- */
  /* ------------------------- Parameters Sanity Check      ------------------------- */
  /* ------------------------- Input / Output               ------------------------- */
  /* ------------------------- Registers                    ------------------------- */

  // Bitstream Register for Holding the configuration
  val configStateReg: Vec[UInt] = RegInit(VecInit(Seq.fill(cfgGroup)(0.U(CONF_CFG_DATA_BITS.W))))

  /* ------------------------- Modules                      ------------------------- */
  /* ------------------------- Wires                        ------------------------- */

  // Decode the config state register to config bitstream
  val configBits: SWCfgBits =
    configStateReg.asTypeOf(new SWCfgBits(inParams.length, outParams.length))
  // Name ths routing signal for debug convience
  for (outIdx <- 0 until numOutput) {
    for (subIdx <- 0 until numSubnet) {
      configBits.routing.get.apply(outIdx * numSubnet + subIdx).suggestName(s"route_output${outIdx}_subnet$subIdx")
    }
  }

  // Pick up part for routing select
  val routeSelects: Seq[UInt] = CompleteOutputSubnetSelect(configBits.routing)
  // Routing Wires
  val routeInputs: Seq[DecoupledIO[CompDataBundle]] =
    Wire(Vec(numInput, Decoupled(new CompDataBundle(compNode))))
  val routeOutputs: Seq[DecoupledIO[CompDataBundle]] =
    Wire(Vec(numOutput, Decoupled(new CompDataBundle(compNode))))

  // Buffering Wires
  val buffersInput: Seq[DecoupledIO[CompDataBundle]] =
    Seq.fill(numOutput)(Wire(DecoupledIO(new CompDataBundle(compNode))))
  val buffersOutput: Seq[DecoupledIO[CompDataBundle]] =
    Seq.fill(numOutput)(Wire(DecoupledIO(new CompDataBundle(compNode))))
  val buffersCount: Seq[UInt] = Seq.fill(numOutput)(WireInit(0.U(depthBits.W)))

  // Dataflow Path Through Signel
  val outputHasSource: Seq[Bool] = Seq.fill(numOutput)(WireInit(false.B))
  val inputHasSink:    Seq[Bool] = Seq.fill(numInput)(WireInit(false.B))
  val inputReadies:    Seq[Bool] = Seq.fill(numInput)(WireInit(false.B))

  /* ------------------------- Combination Logic            ------------------------- */

  /*---------- IO Inputs <-> Routing Inputs ----------*/
  require(routeInputs.length == compInPorts.length, s"Each input will get routed")
  routeInputs.zip(compInPorts).foreach { case (routeIn, bundleIn) =>
    val bool = bundleIn.ctrl.get.dAct.getOrElse(true.B) // power save
    routeIn.valid := bundleIn.valid.get && bool
    CompDataConnect(routeIn.bits, bundleIn.data.get)
    bundleIn.ready.get := routeIn.ready
  }

  /*---------- Routing Outputs <-> Buffering Inputs ----------*/
  // Only Dynamic Switch will route the data input to buffer, static switch will just use output register
  require(buffersInput.length == routeOutputs.length, s"Each SubNet shifted get buffered")
  buffersInput.zip(routeOutputs.zip(compOutPorts)).foreach { case (bufferInput, (routeOutput, output)) =>
    // Use the output control field "uActive" (upstream node active) to gate the valid bit
    val dAct: Bool = output.ctrl.get.uAct.getOrElse(true.B) //power save
    bufferInput.bits.vecData := routeOutput.bits.vecData
    bufferInput.valid := routeOutput.valid && dAct
    routeOutput.ready := bufferInput.ready
  }

  /*---------- Buffer Outputs <-> IO Outputs ----------*/

  // Dynamic Switch will use the output from the buffer queue, and backpressure will be sent to upstream
  require(compOutPorts.length == buffersOutput.length, s"Each output direction should have one output buffer")
  compOutPorts.zip(buffersOutput).foreach { case (output, bufferOutput) =>
    output.valid.get := bufferOutput.valid
    require(
      output.data.get.vecData.length == bufferOutput.bits.vecData.length,
      s"Number of subnet and bit width is problematic"
    )
    output.data.get.vecData.zip(bufferOutput.bits.vecData).foreach { case (v0, v1) =>
      v0 := v1
    }
    bufferOutput.ready := output.ready.get
  }

  /*---------- Internal Logics for Subnet Routing and Output Buffering ----------*/
  // TODO: we should have a better way to switch between FPGA and ASIC backend
  val asicBackend: Boolean = false
  if (asicBackend) {
    // Turn off clock if node is not enabled, reset it when not enabled, to save power
    // TODO: FPGA cannot do multi-clock design like this
    withClockAndReset(
      (configBits.enabled && clock.asBool()).asClock(),
      (!configBits.enabled || reset.asBool()).asAsyncReset()
    ) {
      route()
      buffers()
    }
  } else {
    route()
    buffers()
  }

  /* ------------------------- Finite State Machine         ------------------------- */
  // Config the control state register, updating is based on normal clock
  // TODO: for now switch only have one configuration group, we may have more if we have multiplicity for switch
  val configThis: Bool = configPort.valid && configPort.nodeId === compNode.getNodeId.U &&
    configPort.nodeType === swConfType && configPort.cfgGroup === routeCfgGroup
  when(configThis) {
    configStateReg(configPort.cfgIndex) := configPort.cfgBits
  }

  /* ------------------------- Output Connection            ------------------------- */

  /*---------- Node Status Control ----------*/

  // Report Node Status downward, enabled and output has source goes to it
  require(
    compOutPorts.length == outputHasSource.length,
    s"Output port length = ${compOutPorts.length}, " +
      s"has source length = ${outputHasSource.length}"
  )
  compOutPorts.zip(outputHasSource).foreach { case (output, hasSource) =>
    output.ctrl.get.dAct match {
      case Some(bool) => bool := RegNext(configBits.enabled && hasSource);
      case None       =>
    }
  }

  // Report Node status upward, enabled and input has sink that needs it
  require(
    compInPorts.length == inputHasSink.length,
    s"Input port length = ${compInPorts.length}, " +
      s"has sink length = ${inputHasSink.length}"
  )
  compInPorts.zip(inputHasSink).foreach { case (input, hasSink) =>
    input.ctrl.get.uAct match {
      case Some(bool) => bool := RegNext(configBits.enabled && hasSink);
      case None       =>
    }
  }

  // Busy as node is enabled, and elements in the queues
  compStatus.busy := configBits.enabled && VecInit(buffersOutput.map(_.valid)).asUInt().orR()

  // Connect the config bits aliveness to status port
  compStatus.alive := configBits.enabled

  // Connect config bit to status port
  compStatus.configured := configThis

  // Connect Firing (data is leaving from switch) to status port
  compStatus.fired := VecInit(buffersOutput.map(_.fire())).asUInt().orR()

  // Connect Hardware Type to status port
  compStatus.hwType := swCompType

  /* ------------------------- Hardware Sanity Check        ------------------------- */
  /* ------------------------- Post Generation Sanity Check ------------------------- */
  /* ------------------------- Utility                      ------------------------- */
  /* ------------------------- Derived Parameters           ------------------------- */

  // Calculate how many bits needed for control this node, including node enable
  def csrBits: Int = 1 + routeParam.csrBits(inParams.length, outParams.length)

  // Calculate how many group needed for control this node
  def cfgGroup: Int = csrBits / CONF_CFG_DATA_BITS + {
    if (csrBits % CONF_CFG_DATA_BITS == 0) 0 else 1
  }
}

object SwitchModuleImpl extends App {

  var samples = ListBuffer[(Int, Int, Int, Boolean, Int, Int)]()
  println(s"Generating Values")

  for (input <- 2 until 16) {
    for (output <- 1 until 16) {
      for (depth <- 1 until 10) {
        for (static <- Seq(true, false)) {
          for (datawidth <- Seq(8, 16, 32, 64, 128)) {
            for (granularity <- Seq(8, 16, 32, 64)) {
              if (granularity <= datawidth) {
                samples += ((input, output, depth, static, datawidth, granularity))
              }
            } // End of explore granularity
          } // End of explore datawidth
        } // End of explore dynamic / static
      } // End of explore output buffer
    } // End of explore number of output
  } // End of explore number of input

  val samplesList = scala.util.Random.shuffle(samples.toList)

  println(s"Total number of configurations: ${samplesList.length}")
  println(s"Start to generate the configurations")

  val iter_number = min(100000, samplesList.length)

  for (n <- 0 to iter_number) {
    val (input, output, depth, static, datawidth, granularity) = samplesList(n)
    println(
      s"Generating configuration $n/$iter_number: input=$input, output=$output, depth=$depth, static=$static, datawidth=$datawidth, granularity=$granularity"
    )
    // Configuration with node ID
    val cdeParameter: Parameters = new WithRouting() ++
      new WithCompNode(
        CompNodeParameters(nodeType = Switch, nodeId = 0, compBits = datawidth, compUnitBits = granularity),
        depth,
        static
      )

    // Get the compute node parameter
    val compNode: CompNodeParameters = cdeParameter(CompNode)

    // Reconfiguration protocol
    val reconfParam: CompReconfEdge = CompReconfEdge(p = cdeParameter)

    // Input / Output Parameter
    val inParams: Seq[CompDirEdgeParameters] = Seq.fill(input)(new CompDirEdgeParameters(true, compNode, cdeParameter))
    val outParams: Seq[CompDirEdgeParameters] =
      Seq.fill(output)(new CompDirEdgeParameters(true, compNode, cdeParameter))

    // Generate Verilog
    (new ChiselStage).emitVerilog(
      new SwitchModuleImpl(compNode, reconfParam, inParams, outParams)(cdeParameter),
      Array(
        "--full-stacktrace",
        "--target-dir",
        "/data/dkupsh/switch/data/" + input + "-I_" + output + "-O_" + depth + "-D_" + static + "-S_" + datawidth + "-DW_" + granularity + "-G"
      ),
      Seq(LogLevelAnnotation(LogLevel.Error))
    )
  }
}
