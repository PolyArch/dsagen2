package dsagen2.top.config

import chisel3._
import chisel3.util.{Cat, Enum, log2Ceil}

object DSAFixedConfig {

  // ----- Global Fixed Parameters, which do not depend on input configs -----

  /** ALU Control example:
    * control look up table
    * control | reuse-1 | reuse-2 | reuse-3 | reset-reg1 | reset-reg2 | dis-result1 | dis-result2 | abstain
    * ---------------------------------------------------------------------------------------------------------
    * (00) |    1    |    1    |    0    |     0      |      1     |      1      |      1      | 1
    * (01) |    0    |    1    |    1    |     0      |      1     |      1      |      1      | 1
    * (10) |    0    |    0    |    0    |     1      |      1     |      1      |      1      | 1
    * (11) |    0    |    0    |    1    |     1      |      1     |      1      |      1      | 0
    * So if we have 01 from either ALU's FIRST result or controlled input
    * Action: reuse the second and third operands, reset the second RESETTABLE register, discard all results
    * abstain means that we reuse all operands, do not reset any register, discard all results (do nothing)
    * controlEntryBits = #operand (for reuse) + #resettableRegister (for reset) + #result (for discard) + 1 (abstain)
    * self controlled cannot happen together with abstain : do nothing forever, hardware will not help you check that
    * if abstain is selected: the control mode is either input controled or not controlled
    */
  // Number of control mode
  val NUM_CTRL_TYPE: Int = 3 // No controlled, input controlled, output controlled

  def CTRL_MODE_BITS: Int = log2Ceil(NUM_CTRL_TYPE)

  val noCtrl :: inputCtrl :: outputCtrl :: Nil = Enum(NUM_CTRL_TYPE)

  // Num of Compute Node Type that hardware generator can support
  // PE:0, Switch:1
  val NUM_COMP_TYPE: Int = 2

  def COMP_TYPE_BITS: Int = log2Ceil(NUM_COMP_TYPE)

  val peCompType :: swCompType :: Nil = Enum(NUM_COMP_TYPE)

  // Number of Memory (Engine) Type that hardware generator can support
  // DMA:0; Scratchpad:1; Recurrence:2; Generate:3; Register:4; Discard:5 (deprecated)
  val NUM_MEM_TYPE: Int = 6

  def MEM_TYPE_BITS: Int = log2Ceil(NUM_MEM_TYPE)

  val dmaMemType :: spmMemType :: recMemType :: genMemType :: regMemType :: disMemType :: Nil = Enum(NUM_MEM_TYPE)

  // Memory Operation Encoding
  val NUM_MEM_OP: Int = 8

  def MEM_OP_BITS: Int = log2Ceil(NUM_MEM_OP)

  val (memOpRead :: memOpWrite ::
    memOpAtomOp0 :: memOpAtomOp1 :: memOpAtomOp2 :: memOpAtomOp3 :: memOpAtomOp4 ::
    // this is all one, if you want to use invalid memOperation check,
    // which can also be used as invalid, is you have less than 6 kinds of atomic operation
    memOpAtomOp5 ::
    Nil) = Enum(NUM_MEM_OP)

  // Linear stream filling mode that hardware generator can support
  /** Please be attention: Filling mode can only be supported by at least linear 2d pattern
    * 0: No padding
    * 1: Pad Zero at the End of Whole Stream
    * 2: Pad Off. at the End of Whole Stream
    * 3: Pad Zero at the End of 2D    Stream
    * 4: Pad Off. at the End of 2D    Stream
    * 5: Pad Zero at the End of 1D    Stream
    * 6: Pad Off. at the End of 1D    Stream
    * Example:
    * parameters: initial length1d = 5, stride2d = -2, shrink2d = -1, length2d = 4
    * stream: [
    * [8, 7, 6, 5, 4],
    * [6, 5, 4, 3],
    * [4, 3, 2],
    * [2, 1]
    * ]
    * INPUT VECTOR PORT WIDTH = 4
    *
    * 0. no fill, just normal linear pattern
    *
    * 1. Pad Zero at the End of Whole Stream:
    * [8, 7, 6, 5],
    * [4, 6, 5, 4],
    * [3, 4, 3, 2],
    * [2, 1, 0, 0] // make the last compute vector to fit the required active compute port by filling zero
    *
    * 2. Pad Off. at the End of Whole Stream:
    * [8, 7, 6, 5],
    * [4, 6, 5, 4],
    * [3, 4, 3, 2],
    * [2, 1, X, X] // force send the last two elements to Compute System
    *
    * 3. Pad Zero at the End of 2D    Stream
    * [8, 7, 6, 5],
    * [4, 6, 5, 4],
    * [3, 4, 3, 2],
    * [2, 1, 0, 0] // In this case, the stream itself is a 2D stream, so the same as 1
    *
    * 4. Pad Off. at the End of 2D    Stream
    * [8, 7, 6, 5],
    * [4, 6, 5, 4],
    * [3, 4, 3, 2],
    * [2, 1, X, X] // Like 3
    *
    * 5. Pad Zero at the End of 1D    Stream
    * [8, 7, 6, 5],
    * [4, 0, 0, 0],// Pad zero for the first 1D stream
    * [6, 5, 4, 3],// The second 1D stream just match the vector width, no pad needed
    * [4, 3, 2, 0],// Pad 1 zero for 1D stream whose length is 3
    * [2, 1, 0, 0] // Pad 2 zero for last 1D stream whose length is 2
    *
    * 6. Pad Off. at the End of 1D    Stream
    * [8, 7, 6, 5],
    * [4, X, X, X],// Pad Off for the first 1D stream
    * [6, 5, 4, 3],// The second 1D stream just match the vector width, no pad needed
    * [4, 3, 2, X],// Pad last element to be off for 1D stream whose length is 3
    * [2, 1, X, X] // Pad 2 off for last 1D stream whose length is 2
    */
  val NUM_LINEAR_PADDING: Int = 7

  def LINEAR_PADDING_BITS: Int = log2Ceil(NUM_LINEAR_PADDING)

  val (noPad :: // no padding at all
    // Padding at the end of stream
    padZeroStrEnd :: // fill zero at the end of stream, to be multiple of vector width
    padOffStrEnd :: // fill off at the end of stream, to be multiple of vector width
    // Padding at the end of 2D stream
    padZero2DEnd :: // fill zero at the beginning of each stride, make l1d to be multiple of vector width
    padOff2DEnd :: // fill zero at the end of each stride, make l1d to be multiple of vector width
    // Padding at the end of 1D stream
    padZero1DEnd :: // fill off at the beginning of each stride, make l1d to be multiple of vector width
    padOff1DEnd :: // fill off at the end of each stride, make l1d to be multiple of vector width
    Nil) = Enum(NUM_LINEAR_PADDING)

  // The maximum number of different data types of data stream we can support
  val MAX_NUM_DATA_STREAM_DATATYPE: Int = 4

  def MAX_NUM_DATA_STREAM_DATATYPE_BIT: Int = log2Ceil(MAX_NUM_DATA_STREAM_DATATYPE)

  // The maximum number of different data types of memory stream we can support
  val MAX_NUM_MEM_STREAM_DATATYPE: Int = 4

  def MAX_NUM_MEM_STREAM_DATATYPE_BIT: Int = log2Ceil(MAX_NUM_MEM_STREAM_DATATYPE)

  // Number of Local/Global Sync Input/Output Port that hardware can support
  val MAX_NUM_LOCAL_VPORT: Int = 128 // this is just for input OR output

  def MAX_LOCAL_VPORT_ID_BIT: Int = log2Ceil(MAX_NUM_LOCAL_VPORT)

  val MAX_NUM_GLOBAL_VPORT: Int = MAX_NUM_LOCAL_VPORT * 2 // Input + Output

  def MAX_GLOBAL_VPORT_ID_BIT: Int = log2Ceil(MAX_NUM_GLOBAL_VPORT)

  // Number of lower bits used as Trigonometric Operation
  val TRIG_IN_BITS: Int = 8

  /* --------- DSAGEN Configuration Bitstream Max Size in Byte --------- */
  // Configuration Bitstream is up to 8MB long
  val MAX_CONFIG_BITSTREAM_XLEN_SIZE: Int = 8 * 1024 * 1024

  def MAX_CONFIG_BITSTREAM_SIZE_BITS: Int = log2Ceil(MAX_CONFIG_BITSTREAM_XLEN_SIZE)

  /* ---------- Vector Port Static Parameters ---------- */

  // IVP Config Register
  val NUM_IVP_REG: Int = 3
  val setBroadcast :: setRepeatTime :: setRepeatDelta :: Nil = Enum(NUM_IVP_REG)

  // IVP Repeat Time and Delta Bits
  val VP_REPEAT_FIXPOINT_BITS: Int = 64
  val VP_REPEAT_FRAC_BITS:     Int = 4

  // Maximum capacity of vector in bytes
  val MAX_VP_BYTE: Int = 128

  def MAX_VP_BYTE_BITS: Int = log2Ceil(MAX_VP_BYTE)

  def VP_REPEAT_INT_BITS: Int = VP_REPEAT_FIXPOINT_BITS - VP_REPEAT_FRAC_BITS

  def VP_REPEAT_FIXPOINT_ONE: UInt = Cat(1.U(VP_REPEAT_INT_BITS.W), 0.U(VP_REPEAT_FRAC_BITS.W))

  /* ---------- Compute Reconfiguration Node Static Parameters ---------- */

  // Configuration Bus Protocol
  val CONF_NODE_TYPE:     Int = 4
  val CONF_NODE_ID:       Int = 256
  val CONF_CFG_GROUP:     Int = 4
  val CONF_CFG_IDX:       Int = 16
  val CONF_CFG_DATA_BITS: Int = 48

  // The max register bits in PE
  val CONF_RF_MAX_BITS: Int = 32
  require(
    CONF_RF_MAX_BITS <= CONF_CFG_DATA_BITS,
    s"The bit width of PE register should not be larger than" +
      s"PE configuration data width, since that is where initial register value comes from. But" +
      s"Configure data bits is $CONF_CFG_DATA_BITS, register bits is $CONF_RF_MAX_BITS"
  )

  // Compute Node Type for Compute Node Reconfiguration
  val peConfType :: swConfType :: ivpConfType :: ovpConfType :: Nil = Enum(CONF_NODE_TYPE)

  // Reconfiguration Port Max Fanout
  val CONF_FANOUT: Int = 2

  // Reconfiguration network build max retry
  val MAX_RETRY_CONF: Int = 10000
}
