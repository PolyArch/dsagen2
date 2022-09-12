package dsagen2.mem.module.agu

import chisel3._
import chisel3.util._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.util.StreamUtil.absSignUInt

/** This is a small address generator, may be expensive
  *
  * This module count the number of memory unit requested by using outstanding 1D stream info, limited by memory bandwidth
  * Parameter needed: memory bandwidth (memWidth)
  * Input: rowOffset : UInt, length1D : UInt, dataTypeExp : Option[UInt], stride1D : Option[UInt], as Signed
  * Output: numAcceUnit : UInt, mask : UInt (with #numAcceUnit lower bit = 1)
  */
class OVPRequestMaskGen(memNode: MemNodeParameters) extends MultiIOModule {
  /* ------------------------- Extract Parameters           ------------------------- */

  /* ------------------------- Derived Parameters           ------------------------- */

  /* ------------------------- Parameters Sanity Check      ------------------------- */

  /* ------------------------- Input / Output               ------------------------- */

  // Input
  val address:  UInt = IO(Input(UInt(memNode.addrBits.W)))
  val length1D: UInt = IO(Input(UInt(memNode.length1DBits.W)))
  val dataTypeExp: Option[UInt] = {
    if (memNode.numMemDataTypeExp > 1) Some(IO(Input(UInt(memNode.memDataTypeBits.W)))) else None
  }
  val stride1D: Option[UInt] = {
    if (memNode.MaxAbsStride1D > 0) Some(IO(Input(UInt(memNode.stride1DBits.W)))) else None
  }

  // Output
  val numAccessUnit: UInt = IO(Output(UInt(log2Ceil(memNode.bandwidth).W)))
  val lowerSetMask:  UInt = IO(Output(UInt(memNode.bandwidth.W)))

  /* ------------------------- Registers                    ------------------------- */

  /* ------------------------- Modules                      ------------------------- */

  /* ------------------------- Wires                        ------------------------- */

  /* ------------------------- Combination Logic            ------------------------- */

  val (numAccess, mask): (UInt, UInt) = OVPRequestMaskGen(
    address,
    length1D = length1D,
    dataTypeExp = dataTypeExp,
    stride1D = stride1D,
    memWidth = memNode.bandwidth
  )

  /* ------------------------- Finite State Machine         ------------------------- */

  /* ------------------------- Output Connection            ------------------------- */

  numAccessUnit := numAccess
  lowerSetMask := mask

  /* ------------------------- Hardware Sanity Check        ------------------------- */

  /* ------------------------- Utility                      ------------------------- */

  /* ------------------------- Post Generation Sanity Check ------------------------- */
}

object OVPRequestMaskGen {

  /** ATTENTION: The number of calling this function = #RequestCount Module, can be expensive!!!
    *
    * This module count the number of memory unit requested by using outstanding 1D stream info, limited by memory bandwidth
    *
    * @param address     Address that lower bits means the row offset
    * @param length1D    The linear length 1D
    * @param dataTypeExp Optional Data Type Exponential
    * @param stride1D    Signed Stride1D, when it is 1-bit, it is unsigned
    * @param memWidth    Memory Width
    * @return (UInt[log2(memWidth + 1)], UInt[memWidth]) =
    *         (number of accessed unit, bitmask with number of accessed unit's lower bit = 1)
    */
  def apply(
    address:     UInt,
    length1D:    UInt,
    dataTypeExp: Option[UInt],
    stride1D:    Option[UInt],
    memWidth:    Int
  ): (UInt, UInt) = {
    // Sanity check: memory width need to be larger than 0 and power of two
    require(memWidth > 0 && isPow2(memWidth), s"Memory width needs to be positive and power of two")
    // Create result wire
    val numAccessUnit: UInt = WireInit(0.U(log2Ceil(memWidth + 1).W))
    val mask:          UInt = WireInit(0.U(memWidth.W))
    val nonZero:       Bool = length1D.orR()
    // Edge case
    if (memWidth == 1) {
      numAccessUnit := nonZero
      mask := nonZero
    } else {
      /* ------------------------- Derived Parameters           ------------------------- */

      // Number of bit needed for offset
      val offBits: Int = log2Ceil(memWidth)

      /* ------------------------- Parameters Sanity Check      ------------------------- */

      // Row offset has to be wider than offset bits needed
      require(
        address.getWidth >= offBits,
        s"Row offset is ${address.getWidth}-bit, but memory width = $memWidth," +
          s"which means at least $offBits needed for specifying the row offset"
      )

      // Length 1D has to be wider then offset bits
      require(
        length1D.getWidth >= offBits,
        s"Length 1D is ${length1D.getWidth}-bit, memory width = $memWidth," +
          s"do you mean that we will never access more than one row element?"
      )

      /* ------------------------- Wires                        ------------------------- */

      // C, Get the lower part of row offset
      val rowOff: UInt = address(offBits - 1, 0)

      // C, Absolute value of stride1D
      val absStride1D: UInt = absSignUInt(stride1D.getOrElse(0.U(1.W)))

      // C, Will do access
      val doAccess: Bool = length1D.orR()

      // C, Quick whether length 1D tells us the access will be wider than 1 memory row
      val quickL1DGTWidth: Bool = {
        if (length1D.getWidth == offBits) {
          false.B
        } else {
          // Get the higher bits
          val higherBits = length1D(length1D.getWidth - 1, offBits)
          higherBits.orR()
        }
      }

      // C, data type
      val dtExp: UInt = dataTypeExp.getOrElse(0.U)

      // C, Calculate row offset under data type exp
      val rowOffExp: UInt = (rowOff >> dtExp).asUInt()

      // Calculate the number of accessed element under certain data type exponential for ascending access
      val aAccessNumElemExp: UInt = {
        // Create result wire
        val result: UInt = WireInit(0.U(log2Ceil(memWidth + 1).W))
        // Calculate memory width under data type exp
        val memWidthExp: UInt = (memWidth.U(log2Ceil(memWidth + 1).W) >> dtExp).asUInt()
        // Calculate the number of continuous element from offset to the end
        val numConAccToEnd: UInt = memWidthExp - rowOffExp
        // Calculate the number of access, composed by / and %
        // TODO: hard limit to the width of divider and mod, please only use the bit needed
        val divWidth:     Int = 8.min(absStride1D.getWidth).min(numConAccToEnd.getWidth)
        val divNumAccExp: UInt = numConAccToEnd / absStride1D(divWidth - 1, 0)
        val modNumAccExp: UInt = numConAccToEnd % absStride1D(divWidth - 1, 0)
        // If mod is not equal to 0, then + 1 element is accessed
        val numAccToEnd: UInt = Mux(modNumAccExp === 0.U, divNumAccExp, divNumAccExp + 1.U)
        // Take the minimum between length 1d and numAccToEnd
        val min: UInt = numAccToEnd.min(length1D)
        // Assign to result by using fast L1D larger check
        require(numAccToEnd.getWidth == result.getWidth)
        result := Mux(quickL1DGTWidth, numAccToEnd, min)
        // Return
        Mux(absStride1D === 0.U && length1D > 0.U, 1.U, Mux(length1D === 0.U, 0.U, result))
      }

      // Calculate the number of accessed element under certain data type exponential for descending access
      val dAccessNumElemExp: Option[UInt] = {
        if (stride1D.getOrElse(0.U(1.W)).getWidth > 1) {
          // Create result wire
          val result: UInt = WireInit(0.U(log2Ceil(memWidth + 1).W))
          // Calculate the number of continuous element accessed from 0 to row offset exp
          val numConAccFromStart: UInt = rowOffExp +& 1.U(1.W)
          require(numConAccFromStart.getWidth == log2Ceil(memWidth + 1))
          // Calculate the number of division ceiling
          val divWidth:     Int = 8.min(absStride1D.getWidth).min(numConAccFromStart.getWidth)
          val divNumAccExp: UInt = numConAccFromStart / absStride1D(divWidth - 1, 0)
          val modNumAccExp: UInt = numConAccFromStart % absStride1D(divWidth - 1, 0)
          // If mod is not 0, then add 1
          val numAccFromStart: UInt = Mux(modNumAccExp === 0.U, divNumAccExp, divNumAccExp +& 1.U)
          // Take the minimum between number of accessed and length 1D
          val min: UInt = numAccFromStart.min(length1D)
          // Assign to the result by using fast L1D larger check
          /*          require(result.getWidth == min.getWidth)
                    require(result.getWidth == numAccFromStart.getWidth)*/
          result := Mux(quickL1DGTWidth, numAccFromStart, min)
          // Return
          Some(Mux(absStride1D === 0.U && length1D > 0.U, 1.U, Mux(length1D === 0.U, 0.U, result)))
        } else None
      }

      // Calculate the final number of access element under certain data type by taking direction into account
      val numAccessElemExp: UInt = {
        dAccessNumElemExp match {
          case Some(dNum) =>
            require(stride1D.getOrElse(0.U(1.W)).getWidth > 1)
            val result: UInt = WireInit(0.U(log2Ceil(memWidth + 1).W))
            val s1d:    UInt = stride1D.get
            val isNeg:  Bool = s1d(s1d.getWidth - 1).asBool()
            result := Mux(isNeg, dNum, aAccessNumElemExp)
            result
          case None => aAccessNumElemExp
        }
      }

      // Calculate final number of access unit by taking data type into account
      val numAccessElem: UInt = (numAccessElemExp << dtExp).asUInt()

      // Set the numAccessElem lower bit of mask
      val lowerSetMask: UInt = {
        val bitVec: Seq[Bool] = for (idx <- 0 until memWidth) yield {
          idx.U < numAccessElem
        }
        VecInit(bitVec).asUInt()
      }

      /* ------------------------- Combination Logic            ------------------------- */

      /* ------------------------- Output Connection            ------------------------- */

      // Connect to result wire
      numAccessUnit := numAccessElem
      mask := lowerSetMask

      /* ------------------------- Hardware Sanity Check        ------------------------- */

    }
    (Mux(nonZero, numAccessUnit, 0.U), Mux(nonZero, mask, 0.U))
  }
}
