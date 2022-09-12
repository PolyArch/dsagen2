package dsagen2.comp.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.comp.bundle.{CompDirBundle, CompNodeStatus}
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.reconf.CompReconfEdge
import dsagen2.comp.diplomacy.CompDirEdgeParameters
import dsagen2.top.bundle.ReconfPort
import dsagen2.top.config.DebugPrintable
import dsagen2.util.StreamUtil.nameVecData

abstract class CompModuleImpl extends MultiIOModule with DebugPrintable {
  implicit val p:  Parameters
  val compNode:    CompNodeParameters
  val reconfParam: CompReconfEdge
  val inParams:    Seq[CompDirEdgeParameters]
  val outParams:   Seq[CompDirEdgeParameters]

  /*---------- Parameters ----------*/

  def numInput: Int = {
    require(inParams.nonEmpty)
    inParams.length
  }

  def numOutput: Int = {
    require(outParams.nonEmpty)
    outParams.length
  }

  // Necessary Port for compute node
  val compStatus:   CompNodeStatus = IO(Output(new CompNodeStatus))
  val configPort:   ReconfPort = IO(Flipped(new ReconfPort(reconfParam)))
  val compInPorts:  Seq[CompDirBundle] = inParams.map(in => IO(Flipped(new CompDirBundle(in))))
  val compOutPorts: Seq[CompDirBundle] = outParams.map(out => IO(new CompDirBundle(out)))

  // Print debug info
  if (printDebug) {
    println(s"$numInput Node(s) --> ${compNode.getNodeName} --> $numOutput Node(s)")
  }

  // Debug Wire
  if (printDebug) {
    // Wires that carries all value of vector of tag value that used for debug
    val inputValues: Seq[UInt] = compInPorts.zipWithIndex.map { case (x, idx) =>
      nameVecData(x.data.get.vecData, s"Input${idx}_Value")
    }
    val outputValues: Seq[UInt] = compOutPorts.zipWithIndex.map { case (x, idx) =>
      nameVecData(x.data.get.vecData, s"Output${idx}_Value")
    }
    inputValues.foreach(dontTouch(_)) // Debug
    outputValues.foreach(dontTouch(_)) // Debug
  }
}
