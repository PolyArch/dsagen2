package dsagen2.comp.diplomacy

import dsagen2.comp.config.CompNodeParameters
import freechips.rocketchip.diplomacy._

// Comp Node Nexus Implementation
case class CompNexusNode(
  primaryFn: Seq[CompNodeParameters] => CompNodeParameters,
  replicaFn: Seq[CompNodeParameters] => CompNodeParameters
)(
  implicit valName: ValName)
    extends NexusNode(CompNodeImp)(primaryFn, replicaFn, inputRequiresOutput = false, outputRequiresInput = false)
