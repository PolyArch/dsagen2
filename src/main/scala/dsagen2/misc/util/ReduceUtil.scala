package dsagen2.misc.util

import chisel3._
import chisel3.util._

/** Utility Reduction Function
  */
object ReduceUtil {

  /** Extend mask bit by exponential
    * Example: [0, 0, 0, 0, 0, 1, 0, 1] => exp = 1 => [0, 0, 0, 0, 1, 1, 1, 1]
    * Example: [0, 0, 0, 1, 0, 0, 0, 1] => exp = 1 => [0, 0, 1, 1, 0, 0, 1, 1]
    * Example: [0, 0, 0, 0, 0, 0, 0, 1] => exp = 2 => [0, 0, 0, 0, 1, 1, 1, 1]
    * Example: [0, 0, 0, 0, 0, 0, 0, 1] => exp = 3 => [1, 1, 1, 1, 1, 1, 1, 1]
    *
    * @param mask   Input mask, whose width should be power of 2
    * @param exp    Exponential, can be None, if only one kind of exp is supported
    * @param numExp Number of exponential
    * @return Mask with bit set by exponential
    */
  def extendMaskByExp(mask: UInt, exp: Option[UInt], numExp: Int): UInt = {
    // Derived parameter
    def maxExp: Int = numExp - 1

    def width: Int = mask.getWidth

    // Requirement check
    require(isPow2(mask.getWidth), s"Input mask width ${mask.getWidth} should be power of 2")
    require(
      (exp.isEmpty && numExp == 1) || (exp.isDefined && numExp > 1),
      s"Existence of exp ${exp.isDefined} and number of exp $numExp do not match"
    )
    require(mask.getWidth == (1 << maxExp), s"Width of mask ${mask.getWidth} and max exponential $maxExp should match")

    // Create Bool buffer for each bit to hold ongoing bit
    val bitVector: Seq[Bool] = Seq.fill(width)(WireInit(false.B))

    // Input mask as bools
    val maskBools: Seq[Bool] = mask.asBools()

    // Exponential as UInt
    val e: UInt = exp.getOrElse(0.U)

    // Loop from lowest position to highest
    for (unitIdx <- 0 until width) {
      // The LSB should just be passed
      if (unitIdx == 0) bitVector(unitIdx) := maskBools.head
      else {
        // Calculate the max exponential that will affect it
        // unitIdx    : 7 6 5 4 3 2 1 0
        // maxLocalExp: 3 3 3 3 2 2 1 0
        val maxLocalExp: Int = log2Floor(unitIdx) + 1
        val maxIdxDiff:  Int = unitIdx % (1 << maxLocalExp)
        val lowestBit:   Bool = bitVector(unitIdx - maxIdxDiff)
        // A hard coded sanity check
        val sanityMap: Map[Int, Int] = Map(
          0 -> 0,
          1 -> 1,
          2 -> 2,
          3 -> 2,
          4 -> 3,
          5 -> 3,
          6 -> 3,
          7 -> 3
        )
        require(
          maxLocalExp == sanityMap(unitIdx),
          s"$unitIdx's maxLocalExp should be ${sanityMap(unitIdx)}," +
            s"but it is $maxLocalExp"
        )
        // Construct exponential 2 actual bit mapping
        val exp2bit: Seq[(UInt, Bool)] = for (exp <- 0 to maxLocalExp) yield {
          // Calculate the position difference
          val idxDiff: Int = unitIdx % (1 << exp)
          // Point to the position that is lower by idxDiff
          exp.U -> {
            if (idxDiff != 0) bitVector(unitIdx - idxDiff) else maskBools(unitIdx)
          }
        }
        // MuxLookUp mapping to get the local bit
        val localBit: Bool = MuxLookup(e, lowestBit, exp2bit)
        // Add the bit to bit buffer
        bitVector(unitIdx) := localBit
      }
    }

    // Output
    VecInit(bitVector).asUInt()
  }

}
