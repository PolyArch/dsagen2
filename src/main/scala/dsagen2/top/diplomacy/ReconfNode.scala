package dsagen2.top.diplomacy

import dsagen2.comp.config.reconf.CompReconfProto
import dsagen2.comp.diplomacy.CompReconfImp
import freechips.rocketchip.diplomacy.{NexusNode, ValName}

// Configuration Node of Compute Node that connects all config ports of compute node
// The bundle protocol is fixed, there is no parameter negotiation.
case class ReconfNode(
  dsaModule: DSANodeModule,
  pFn:       Seq[CompReconfProto] => CompReconfProto,
  rFn:       Seq[CompReconfProto] => CompReconfProto
)(
  implicit valName: ValName)
    extends NexusNode(CompReconfImp)(pFn, rFn, inputRequiresOutput = false, outputRequiresInput = false)
