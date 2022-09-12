package dsagen2.comp.impl.ip

import chisel3._
import chisel3.util._

class FAddSub_D64_IO extends Bundle {
  // Clock
  val aclk: Bool = Input(Bool())
  // Operands
  val s_axis_a_tvalid: Bool = Input(Bool())
  val s_axis_a_tready: Bool = Output(Bool())
  val s_axis_a_tdata:  UInt = Input(UInt(64.W))
  val s_axis_b_tvalid: Bool = Input(Bool())
  val s_axis_b_tready: Bool = Output(Bool())
  val s_axis_b_tdata:  UInt = Input(UInt(64.W))
  // Operation
  val s_axis_operation_tvalid: Bool = Input(Bool())
  val s_axis_operation_tready: Bool = Output(Bool())
  val s_axis_operation_tdata:  UInt = Input(UInt(8.W))
  // Result
  val m_axis_result_tvalid: Bool = Output(Bool())
  val m_axis_result_tready: Bool = Input(Bool())
  val m_axis_result_tdata:  UInt = Output(UInt(64.W))
}

class FAddSub_D64(val stage: Int = 3) extends BlackBox with HasBlackBoxResource {
  val moduleName: String = s"FAddSub_D64_${stage}stage"

  override def desiredName: String = moduleName

  val io: FAddSub_D64_IO = IO(new FAddSub_D64_IO)
  addResource(blackBoxResource = s"/fpga/$moduleName/sim/$moduleName.v")
}
