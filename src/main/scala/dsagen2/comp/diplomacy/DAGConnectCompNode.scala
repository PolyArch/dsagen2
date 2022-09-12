package dsagen2.comp.diplomacy

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.bundle.CompBDirBundle
import dsagen2.comp.bundle.DirectionalConnection.{DownwardConnection, UpwardConnection}
import dsagen2.comp.config.CompNodeParameters
import dsagen2.sync.module.{IVPNodeModule, OVPNodeModule}
import freechips.rocketchip.diplomacy.MixedNode

trait DAGConnectCompNode {
  val globalCompId: Int
  implicit val p:   Parameters
  val node: MixedNode[
    CompNodeParameters,
    CompNodeParameters,
    CompBDirEdgeParameters,
    CompBDirBundle,
    CompNodeParameters,
    CompNodeParameters,
    CompBDirEdgeParameters,
    CompBDirBundle
  ]

  /* ---------- Connection Between Compute Node ---------- */

  def <--(that: DAGConnectCompNode)(implicit p: Parameters, sourceInfo: SourceInfo) =
    if (this.globalCompId < that.globalCompId) {
      that.node.:=(this.node)(p ++ new UpwardConnection, sourceInfo)
    } else if (this.globalCompId > that.globalCompId) {
      this.node.:=(that.node)(p ++ new DownwardConnection, sourceInfo)
    } else {
      require(requirement = false, s"Same global Id node connection, cannot determine direction")
      this.node.:=(that.node)(p, sourceInfo)
    }

  def -->(that: DAGConnectCompNode)(implicit p: Parameters, sourceInfo: SourceInfo) =
    if (this.globalCompId < that.globalCompId) {
      that.node.:=(this.node)(p ++ new DownwardConnection, sourceInfo)
    } else if (this.globalCompId > that.globalCompId) {
      this.node.:=(that.node)(p ++ new UpwardConnection, sourceInfo)
    } else {
      require(requirement = false, s"Same global Id node connection, cannot determine direction")
      this.node.:=(that.node)(p, sourceInfo)
    }

  def <->(that: DAGConnectCompNode)(implicit p: Parameters, sourceInfo: SourceInfo) =
    if (this.globalCompId < that.globalCompId) {
      that.node.:=(this.node)(p ++ new UpwardConnection ++ new DownwardConnection, sourceInfo)
    } else if (this.globalCompId > that.globalCompId) {
      this.node.:=(that.node)(p ++ new UpwardConnection ++ new DownwardConnection, sourceInfo)
    } else {
      require(requirement = false, s"Same global Id node connection, cannot determine direction")
      this.node.:=(that.node)(p, sourceInfo)
    }

  /* ---------- Connection Between Input Vector Node ----------*/
  // Compute node can only receive connection from IVP, not reversed
  def <--(that: IVPNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) = {
    this.node.:=(that.node)(p ++ new DownwardConnection, sourceInfo)
  }

  /* ---------- Connection Between Output Vector Node ----------*/
  // Compute node can only connect to OVP, not reversed
  def -->(that: OVPNodeModule)(implicit p: Parameters, sourceInfo: SourceInfo) = {
    // Since Connection between compute node and sync node is directional, it always flow from compute to ovp
    that.node.:=(this.node)(p ++ new DownwardConnection, sourceInfo)
  }
}
