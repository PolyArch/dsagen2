package dsagen2.ctrl.module

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import dsagen2.ctrl.bundle.decode.{DataTypeExponential, IndirectPorts}
import dsagen2.ctrl.config.DSARegister._
import dsagen2.top.config.DSAFixedConfig.{MAX_CONFIG_BITSTREAM_SIZE_BITS, MAX_LOCAL_VPORT_ID_BIT}
import dsagen2.top.module.DSAGen
import freechips.rocketchip.tile.XLen

/** DSA Register File Architecture
  */
trait DSARFArch {

  /* ---------- Extract parameters ---------- */
  implicit val p: Parameters
  val dsa:        DSAGen
  val XLEN: Int = p(XLen)

  /* ---------- Register File Architecture ----------*/

  // Initialize the DSA Register File as Floating Register Array
  /** Why this Spec Register File is an array of floating register not a regular nrnw-SRAM? Because we can update up to
    * two register per cycle by using the command ss_para_cfg, but we need to read multiple register when stream instantiation
    * is triggered (like linear 3D stream needs parameters from multiple registers). Also, we only have about 20 registers
    * so it is not that expensive to have an array of floating registers.
    */
  val DSARegFile: Array[Option[UInt]] = Array.fill(maxNumDSARegister)(None)
  // Stickiness of DSA register
  val DSARegFileStickiness: Array[Option[Bool]] = Array.fill(maxNumDSARegister)(None)

  /* --- Control Register per DSAGEN --- */

  // Mandatory
  // Constant Zero, necessary, just ground wire, so it is very cheap
  DSARegFile(dsaRegZero.id) = Some(WireDefault(0.U(XLEN.W)))
  DSARegFileStickiness(dsaRegZero.id) = Some(WireDefault(false.B))
  suggestName(DSARegFile(dsaRegZero.id), DSARegFileStickiness(dsaRegZero.id), dsaRegZero)

  // Optional
  // To Be Configured (TBC register), TODO: we have not support this register yet
  if (false) {
    DSARegFile(dsaRegTBC.id) = Some(RegInit(0.U(XLEN.W)))
    DSARegFileStickiness(dsaRegTBC.id) = Some(RegInit(false.B))
  }

  // Mandatory
  // Configuration bitstream start address, otherwise Compute System will not be reconfigurable
  // TODO: The bit width should not be XLEN (well, it can be, but not best way). The best way is to specify the memory
  //        range, that it can be accessed by DSAGEN and use log2Ceil to calculate it
  DSARegFile(dsaRegCSA.id) = Some(RegInit(0.U(XLEN.W)))
  DSARegFileStickiness(dsaRegCSA.id) = Some(RegInit(false.B))
  suggestName(DSARegFile(dsaRegCSA.id), DSARegFileStickiness(dsaRegCSA.id), dsaRegCSA)

  // Mandatory
  // Configuration bitstream size (in XLEN-bit), mandatory, as above
  DSARegFile(dsaRegCFS.id) = Some(RegInit(0.U(MAX_CONFIG_BITSTREAM_SIZE_BITS.W)))
  // Stickiness usually when you refresh the compute substrate, this will be changed
  DSARegFileStickiness(dsaRegCFS.id) = Some(RegInit(false.B))
  suggestName(DSARegFile(dsaRegCFS.id), DSARegFileStickiness(dsaRegCFS.id), dsaRegCFS)

  // Mandatory
  // Exponential, otherwise we cannot set the data type of stream
  // TODO: we should expose reconfigurable thing to SS-ISA, but now we have 5 data types to support, so in total 10-bit
  DSARegFile(dsaRegExp.id) = Some(RegInit(0.U(10.W)))
  DSARegFileStickiness(dsaRegExp.id) = Some(RegInit(false.B))
  suggestName(DSARegFile(dsaRegExp.id), DSARegFileStickiness(dsaRegExp.id), dsaRegExp)

  /* --- Stream Base Parameters --- */

  // Mandatory
  // stream start address
  DSARegFile(dsaRegStreamStartAddr.id) = Some(RegInit(0.U(XLEN.W)))
  DSARegFileStickiness(dsaRegStreamStartAddr.id) = Some(RegInit(false.B))
  suggestName(
    DSARegFile(dsaRegStreamStartAddr.id),
    DSARegFileStickiness(dsaRegStreamStartAddr.id),
    dsaRegStreamStartAddr
  )

  // Optional
  // Stride 1D, distance between inner most loop access as stride 1D
  if (dsa.maxStride1DBits > 0) {
    DSARegFile(dsaRegStreamStride1D.id) = Some(RegInit(0.U(dsa.maxStride1DBits.W)))
    DSARegFileStickiness(dsaRegStreamStride1D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamStride1D.id),
      DSARegFileStickiness(dsaRegStreamStride1D.id),
      dsaRegStreamStride1D
    )
  }

  // Optional
  // Length 1D, inner most stream length
  if (dsa.maxLength1DBits > 0) {
    DSARegFile(dsaRegStreamLength1D.id) = Some(RegInit(0.U(dsa.maxLength1DBits.W)))
    DSARegFileStickiness(dsaRegStreamLength1D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamLength1D.id),
      DSARegFileStickiness(dsaRegStreamLength1D.id),
      dsaRegStreamLength1D
    )

  }

  /* --- Linear 2D Stream Parameters --- */

  // Optional
  // Stretch 2D, linear 2d stream stretch 2d
  if (dsa.maxStretch2DBits > 0) {
    DSARegFile(dsaRegStreamStretch2D.id) = Some(RegInit(0.U(dsa.maxStretch2DBits.W)))
    DSARegFileStickiness(dsaRegStreamStretch2D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamStretch2D.id),
      DSARegFileStickiness(dsaRegStreamStretch2D.id),
      dsaRegStreamStretch2D
    )
  }

  // Optional
  // Stride 2D, linear 2d stream stride 2d
  if (dsa.maxStride2DBits > 0) {
    DSARegFile(dsaRegStreamStride2D.id) = Some(RegInit(0.U(dsa.maxStride2DBits.W)))
    DSARegFileStickiness(dsaRegStreamStride2D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamStride2D.id),
      DSARegFileStickiness(dsaRegStreamStride2D.id),
      dsaRegStreamStride2D
    )
  }

  // Optional
  // Length 2D, linear 2d stream length
  if (dsa.maxLength2DBits > 0) {
    DSARegFile(dsaRegStreamLength2D.id) = Some(RegInit(0.U(dsa.maxLength2DBits.W)))
    DSARegFileStickiness(dsaRegStreamLength2D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamLength2D.id),
      DSARegFileStickiness(dsaRegStreamLength2D.id),
      dsaRegStreamLength2D
    )
  }

  // Optional
  // linear 3d stream: delta stretch 2d
  if (dsa.maxDeltaStretch2DBits > 0) {
    DSARegFile(dsaRegStreamDeltaStretch2D.id) = Some(RegInit(0.U(dsa.maxDeltaStretch2DBits.W)))
    DSARegFileStickiness(dsaRegStreamDeltaStretch2D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamDeltaStretch2D.id),
      DSARegFileStickiness(dsaRegStreamDeltaStretch2D.id),
      dsaRegStreamDeltaStretch2D
    )
  }

  // Optional
  // linear 3d stream: delta stride 2d
  if (dsa.maxDeltaStride2DBits > 0) {
    DSARegFile(dsaRegStreamDeltaStride2D.id) = Some(RegInit(0.U(dsa.maxDeltaStride2DBits.W)))
    DSARegFileStickiness(dsaRegStreamDeltaStride2D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamDeltaStride2D.id),
      DSARegFileStickiness(dsaRegStreamDeltaStride2D.id),
      dsaRegStreamDeltaStride2D
    )
  }

  // Optional
  // linear 3d stream: stretch 3d to 2d
  if (dsa.maxStretch3D2DBits > 0) {
    DSARegFile(dsaRegStreamStretch3D2D.id) = Some(RegInit(0.U(dsa.maxStretch3D2DBits.W)))
    DSARegFileStickiness(dsaRegStreamStretch3D2D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamStretch3D2D.id),
      DSARegFileStickiness(dsaRegStreamStretch3D2D.id),
      dsaRegStreamStretch3D2D
    )

  }

  // Optional
  // linear 3d stream: stretch 3d to 1d
  if (dsa.maxStretch3D1DBits > 0) {
    DSARegFile(dsaRegStreamStretch3D1D.id) = Some(RegInit(0.U(dsa.maxStretch3D1DBits.W)))
    DSARegFileStickiness(dsaRegStreamStretch3D1D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamStretch3D1D.id),
      DSARegFileStickiness(dsaRegStreamStretch3D1D.id),
      dsaRegStreamStretch3D1D
    )
  }

  // Optional
  // linear 3d stream: stride 3d
  if (dsa.maxStride3DBits > 0) {
    DSARegFile(dsaRegStreamStride3D.id) = Some(RegInit(0.U(dsa.maxStride3DBits.W)))
    DSARegFileStickiness(dsaRegStreamStride3D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamStride3D.id),
      DSARegFileStickiness(dsaRegStreamStride3D.id),
      dsaRegStreamStride3D
    )
  }

  // Optional
  // linear 3d stream length
  if (dsa.maxLength3DBits > 0) {
    DSARegFile(dsaRegStreamLength3D.id) = Some(RegInit(0.U(dsa.maxLength3DBits.W)))
    DSARegFileStickiness(dsaRegStreamLength3D.id) = Some(RegInit(false.B))
    suggestName(
      DSARegFile(dsaRegStreamLength3D.id),
      DSARegFileStickiness(dsaRegStreamLength3D.id),
      dsaRegStreamLength3D
    )
  }

  // Optional
  // indirect ports: index port, stride2d port, length1d port
  if (dsa.supportIndirect) {
    DSARegFile(dsaRegIndirectPorts.id) = Some(RegInit(0.U((3 * MAX_LOCAL_VPORT_ID_BIT).W)))
    DSARegFileStickiness(dsaRegIndirectPorts.id) = Some(RegInit(false.B))
    suggestName(DSARegFile(dsaRegIndirectPorts.id), DSARegFileStickiness(dsaRegIndirectPorts.id), dsaRegIndirectPorts)
  }

  // Optional
  // buffet address: higher part is end address, lower part is start address of assigned buffet
  if (dsa.supportBuffet) {
    DSARegFile(dsaRegBuffetAddr.id) = Some(RegInit(0.U(XLEN.W)))
    DSARegFileStickiness(dsaRegBuffetAddr.id) = Some(RegInit(false.B))
    suggestName(DSARegFile(dsaRegBuffetAddr.id), DSARegFileStickiness(dsaRegBuffetAddr.id), dsaRegBuffetAddr)
  }

  // Optional
  // buffet states:

  /* ---------- Utility ----------*/
  // Configure Register File with Sticky is given
  def writeReg(dsaRegIdx: UInt, value: UInt, stick: Bool, enable: Bool): Unit = {
    for (regIdx <- 0 until maxNumDSARegister) {
      (DSARegFile(regIdx), DSARegFileStickiness(regIdx)) match {
        case (Some(regValue), Some(regStickiness)) =>
          // If register and stickiness are both defined, update it
          when(enable && dsaRegIdx === regIdx.U) {
            regValue := value
            regStickiness := stick
          }
        case (None, None) =>
        // If nothing is defined, ignore it
        case _ =>
          require(requirement = false, s"The definition of register value and register stickiness is different")
      }
    }
  }

  // Configure Register File with stickiness is TRUE
  def writeReg(dsaRegIdx: UInt, value: UInt, enable: Bool): Unit = {
    writeReg(dsaRegIdx, value, true.B, enable)
  }

  // Get the exponential register
  private def getExp: DataTypeExponential = DSARegFile(dsaRegExp.id).get.asTypeOf(new DataTypeExponential)

  // Get the memory stream data type
  def memDataTypeExp: UInt = getExp.memUnitWidthExp

  // Get the constant stream data type
  def constDataTypeExp: UInt = getExp.constStrDataTypeExp

  // Get the index stream data type
  def idxStrDataTypeExp: UInt = getExp.indexStrDataTypeExp

  // Get the Stride 2D stream data type
  def s2dStrDataTypeExp: UInt = getExp.stride2DStrDataTypeExp

  // Get the length 1D stream data type
  def l1dStrDataTypeExp: UInt = getExp.length1DStrDataTypeExp

  // Get Bundle of Indirect Ports
  private def getIndirectPorts: Option[IndirectPorts] = {
    DSARegFile(dsaRegIndirectPorts.id) match {
      case Some(rawUInt) =>
        require(
          rawUInt.getWidth == (3 * MAX_LOCAL_VPORT_ID_BIT),
          s"Indirect Ports Register exists, " +
            s"where encode three indirect output vector port. In total should be 3 * $MAX_LOCAL_VPORT_ID_BIT = " +
            s"${3 * MAX_LOCAL_VPORT_ID_BIT}-bit, but it is ${rawUInt.getWidth}-bit"
        )
        Some(rawUInt.asTypeOf(new IndirectPorts))
      case None => None
    }
  }

  // Get the index port Id
  def indexPortId: Option[UInt] =
    getIndirectPorts match {
      case Some(indPorts) => Some(indPorts.indexPortId)
      case None           => None
    }

  // Get the stride 2d stream indirect port id
  def stride2DPortId: Option[UInt] =
    getIndirectPorts match {
      case Some(indPorts) => Some(indPorts.stride2DPortId)
      case None           => None
    }

  // Get the stride 2d stream indirect port id
  def length1DPortId: Option[UInt] =
    getIndirectPorts match {
      case Some(indPorts) => Some(indPorts.length1DPortId)
      case None           => None
    }

  // Name Register in Chisel
  def suggestName(reg: Option[UInt], stickiness: Option[Bool], name: DSARegister): Unit = {
    reg match {
      case Some(value) => value.suggestName(name.toString)
      case None        =>
    }
    stickiness match {
      case Some(value) => value.suggestName(name.toString + "stickiness")
      case None        =>
    }
  }
}
