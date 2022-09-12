package dsagen2.mem.module.agu

import chisel3._
import chisel3.util._
import dsagen2.mem.config.MemNodeParameters

object LinearStreamAGU {

  /** Return Bitmask whose width == memWidth, numElementAccess under dataType
    * Attention: The number of calling this function == #AGU created, AGU can be expensive in terms of hardware
    *
    * @param startAddr      Current Address
    * @param Length1D       Length 1D, 0.UInt means that the current access will be the last one
    * @param memDataTypeExp Memory Type, 0 means byte, 1 means half-wore, 2 means 32-bit
    * @param stride1D       Possible Stride 1D
    * @param memNode        Memory Node Parameters
    * @return Return access bitmask whose width is equal to memWidth, number of accessed element (not byte, which means
    *         data type need to be considered) in this access
    */
  def LinearMaskGen(
    startAddr:      UInt,
    Length1D:       UInt,
    memDataTypeExp: Option[UInt],
    stride1D:       Option[UInt],
    memNode:        MemNodeParameters
  ): (UInt, UInt) = {

    /* ---------- Derived Parameters ---------- */
    val memWidth:       Int = memNode.bandwidth
    val MaxAbsStride1D: Int = memNode.MaxAbsStride1D
    val numMemDataType: Int = memNode.numMemDataTypeExp

    def offsetBits: Int = {
      require(memWidth > 1, s"memory width == 1? read one byte at a time?")
      log2Ceil(memWidth)
    }

    /* ---------- Sanity Check -----------*/

    // Sanity Check: Memory Width should be positive
    require(memWidth > 0, s"Memory Width = $memWidth")

    // Sanity Check: max data type should not be wider than memory width
    require(
      memWidth >= (1 << (numMemDataType - 1)),
      s"Memory Width = $memWidth-byte, maxDataType = ${1 << (numMemDataType - 1)}"
    )

    /* ---------- Wires ---------- */

    // Create result bitmask wire
    val bitmask: UInt = WireInit(0.U(memWidth.W))

    // Create number of element accessed
    val numAccessElement: UInt = WireInit(0.U(log2Ceil(memWidth + 1).W))

    // Zero check for length1D
    val zeroL1D: Bool = !Length1D.orR()

    /* ---------- Combinational Logic: Generate Bit Mask ---------- */

    if (memWidth > 1) { // First we generate local bitmask for each data type
      val localBitmasks: Seq[Vec[Bool]] = for (dataTypeExp <- 0 until numMemDataType) yield {
        // Calculate how many bit for the local bitmask
        val localMaskBits: Int = memWidth >> dataTypeExp
        // Create the local bit mask result wire
        val localBitmaskWire: Vec[Bool] = WireInit(VecInit(Seq.fill(localMaskBits)(false.B)))
        // Calculate the local position pointed by the startAddress
        val localPosition: UInt = if (offsetBits - 1 < dataTypeExp) 0.U(1.W) else startAddr(offsetBits - 1, dataTypeExp)

        // Generate ascending "filter" if positive stride1D exist based on stride1D and L1D
        val ascendRuler: Option[Seq[Bool]] = if (memNode.MaxAbsStride1D > 0) {
          // positive exists means that stride1d should exist
          require(stride1D.isDefined, s"Stride1D should exist")
          val s1d: UInt = stride1D.get
          // Generate mask based on Stride1D
          val maskS1D: Seq[Bool] = for (rulerIdx <- 0 until localMaskBits) yield {
            if (rulerIdx == 0) {
              Length1D.orR()
            } else {
              if (memNode.MaxAbsStride1D == 1) {
                // If max Stride1D == 1 (means that Stride1Ds = -1, 0, 1 (descend, const, ascend))
                // then maskS1D will be all true
                true.B
              } else {
                rulerIdx.S % s1d.asSInt() === 0.S
              }
            }
          }
          // Generate mask based on Length1D
          val maskL1D: Seq[Bool] = for (rulerIdx <- 0 until localMaskBits) yield {
            if (rulerIdx == 0) {
              Length1D.orR() // 0 position will always be true as above
            } else {
              // Get previous subsequence of maskS1D
              val subMaskS1D: Seq[Bool] = maskS1D.slice(0, rulerIdx)
              // Get the number of true in previous subsequence
              val numPrevTrue: UInt = PopCount(subMaskS1D)
              // Check whether it is falls in L1D range
              numPrevTrue < Length1D
            }
          }
          // And this two mask to get ascendRuler
          Some(maskS1D.zip(maskL1D).map(t => t._1 && t._2))
        } else None
        // Descending ruler is just reverse of ascending ruler
        if (ascendRuler.isDefined) require(memNode.MaxAbsStride1D > 0)
        val descendRuler: Option[Seq[Bool]] = if (memNode.MaxAbsStride1D > 1) {
          require(ascendRuler.isDefined)
          Some(ascendRuler.get.reverse)
        } else None

        // Generate ascending result local bitmask
        val ascendResult: Option[Vec[Bool]] = ascendRuler match {
          case Some(aRuler) =>
            // Make the ruler as UInt
            val aRulerUInt: UInt = VecInit(aRuler).asUInt()
            // Dynamic Left Shift by local position
            val aRulerShift: UInt = (aRulerUInt << localPosition).asUInt()
            // Only take the lower local mask bits
            val aLocalMask: UInt = aRulerShift(localMaskBits - 1, 0)
            // Return as vector of Bool
            Some(VecInit(aLocalMask.asBools()))
          case None => None
        }

        // Generate descending result local bitmask
        val descendResult: Option[Vec[Bool]] = descendRuler match {
          case Some(dRuler) =>
            // Make the ruler as UInt
            val dRulerUInt: UInt = VecInit(dRuler).asUInt()
            // Calculate the amount to do right shift
            val reverPosition: UInt = (localMaskBits - 1).U - localPosition
            // Dynamic Right Shift by local position
            val dRulerShift: UInt = (dRulerUInt >> reverPosition).asUInt()
            // Only take the local mask bits
            val dLocalMask: UInt = dRulerShift(localMaskBits - 1, 0)
            // Return as vector of bool
            Some(VecInit(dLocalMask.asBools()))
          case None => None
        }

        // Generate const result
        val constResult: Vec[Bool] = VecInit(
          for (idx <- 0 until localMaskBits) yield idx.U === localPosition
        )

        // Mux result
        val localResult: UInt = {
          if (memNode.MaxAbsStride1D > 1) {
            require(stride1D.isDefined && descendResult.isDefined && ascendResult.isDefined)
            val signedStride1D: SInt = stride1D.get.asSInt()
            val outRange: Bool = (signedStride1D <= (-memWidth).S(signedStride1D.getWidth.W)) ||
              (signedStride1D >= memWidth.S(signedStride1D.getWidth.W))
            Mux(
              outRange,
              constResult.asUInt(), // If access out of range, then it must be const
              Mux(
                signedStride1D < 0.S,
                descendResult.get.asUInt(), // Negative mask
                Mux(
                  signedStride1D > 0.S,
                  ascendResult.get.asUInt(), // Ascend mask
                  constResult.asUInt()
                ) // Const mask
              )
            )
          } else if (memNode.MaxAbsStride1D > 0) {
            require(stride1D.isDefined && ascendResult.isDefined && descendResult.isEmpty)
            Mux(stride1D.get === 0.U, constResult.asUInt(), ascendResult.get.asUInt())
          } else {
            constResult.asUInt()
          }
        }

        // Sanity check: bitwidth is equal
        require(
          localBitmaskWire.length == localResult.getWidth,
          s"LocalWire = ${localBitmaskWire.length}-bit, localResult = ${localResult.getWidth}-bit"
        )
        // Connect to local bitmask wire
        localBitmaskWire.zip(localResult.asBools()).foreach { case (wire, bitResult) =>
          wire := bitResult
        }
        // Return
        localBitmaskWire
      }

      // Secondly,  we popcount the localbitmask for the number of access
      val localNumAccesses: Seq[UInt] = localBitmasks.map(PopCount(_))

      // Thirdly, we convert the local bitmask to global mask whose width is memWidth
      val globalBitmask: Seq[Vec[Bool]] = for (dataTypeExp <- 0 until numMemDataType) yield {
        // Create global bitmask wire
        val globalBitmaskWire: Vec[Bool] = WireInit(VecInit(Seq.fill(memWidth)(false.B)))
        // Get the local bitmask
        val localBitmask: Vec[Bool] = localBitmasks(dataTypeExp)
        // Sanity Check: width should make sense
        require(
          globalBitmaskWire.length / (1 << dataTypeExp) == localBitmask.length,
          s"DataTypeExp = $dataTypeExp, globalMask = ${globalBitmaskWire.getWidth}-bit, " +
            s"localMask = ${localBitmask.length}-bit"
        )
        // Connect to the global wire from local bitmask
        for (globalIdx <- 0 until memWidth) {
          globalBitmaskWire(globalIdx) := localBitmask(globalIdx >> dataTypeExp)
        }
        // Return the global mask wire
        globalBitmaskWire
      }

      // Last, select the result based on dataType
      val bitmaskLUT: Seq[(UInt, UInt)] = for (exp <- 0 until numMemDataType) yield {
        exp.U -> globalBitmask(exp).asUInt()
      }
      val numAccessLUT: Seq[(UInt, UInt)] = for (exp <- 0 until numMemDataType) yield {
        val numAccessWire: UInt = WireInit(0.U(log2Ceil(memWidth + 1).W))
        numAccessWire := localNumAccesses(exp)
        exp.U -> numAccessWire
      }

      // Assign
      bitmask := MuxLookup(memDataTypeExp.getOrElse(0.U), 0.U, bitmaskLUT)
      numAccessElement := MuxLookup(memDataTypeExp.getOrElse(0.U), 0.U, numAccessLUT)

      // Return
      (Mux(zeroL1D, 0.U, bitmask), Mux(zeroL1D, 0.U, numAccessElement))
    } else {
      (Mux(zeroL1D, 0.U, 1.U(1.W)), Mux(zeroL1D, 0.U, 1.U(1.W)))
    }
  }

}
