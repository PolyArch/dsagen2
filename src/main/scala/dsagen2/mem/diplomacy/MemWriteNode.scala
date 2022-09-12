package dsagen2.mem.diplomacy

import dsagen2.mem.config.MemNodeParameters
import dsagen2.sync.config.{IVPNodeParameters, OVPNodeParameters}
import freechips.rocketchip.diplomacy.{MixedNexusNode, SinkNode, ValName}

/** This memory write node is actually a sink node, the reason we cannot use [[SinkNode]]
  * is that [[SinkNode]] requires a pre-defined number of inward links which we don't want. We want use the topology
  * to define the number of inward edge
  */
case class MemWriteNode(
  write2readFn: Seq[OVPNodeParameters] => MemNodeParameters,
  read2writeFn: Seq[IVPNodeParameters] => MemNodeParameters
)(
  implicit valName: ValName)
    extends MixedNexusNode(MemOVPSideNodeImp, MemIVPSideNodeImp)(
      dFn = write2readFn, // This is write node, the write to read mapping is useless
      uFn = read2writeFn, // Write node parameter is defined by itself, not by mapping
      inputRequiresOutput = false, // This is actually sink node, so no outputs required
      outputRequiresInput = false // There is no output, this argument is useless
    ) {}
