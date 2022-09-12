package dsagen2.mem.diplomacy

import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.{IVPNodeParameters, OVPNodeParameters}
import freechips.rocketchip.diplomacy.{MixedNexusNode, SourceNode, ValName}

/** This Memory Read Node is actually a source node, the reason we cannot use [[SourceNode]] is that we want to use the topology
  * to define the number of outgoing edge instead of predefine it
  *
  * @param write2readFn A dummy function will never be used, the output vector port parameter will end at Memory Write Node
  * @param read2writeFn A dummy function that will never be used, the input vector port parameter will end at here
  * @param valName      I don't what does this used for, but it seems that we have to have it
  */
case class MemReadNode(
  write2readFn: Seq[OVPNodeParameters] => MemNodeParameters,
  read2writeFn: Seq[IVPNodeParameters] => MemNodeParameters
)(
  implicit valName: ValName)
    extends MixedNexusNode(MemOVPSideNodeImp, MemIVPSideNodeImp)(
      dFn = write2readFn, // Memory Read Parameter comes from itself, this mapping is useless
      uFn = read2writeFn, // This is read node, write parameter is useless
      inputRequiresOutput = false, // there will be no input for memory read node, so just false
      outputRequiresInput = false // This is an source node, so output does not require input
    ) {}
