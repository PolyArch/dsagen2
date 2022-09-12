package dsagen2.comp.config.switch

import chipsalliance.rocketchip.config.Parameters
import chisel3.util.log2Ceil
import dsagen2.comp.config.CompKeys.{CompNode, SwitchRouting}
import dsagen2.comp.config.ReconfParameters
import dsagen2.util.{JSONParsableConstructor, JSONParsableParameters}
import play.api.libs.json.{JsValue, Json, Writes}

/** Switch Routing Parameters depends on number of input and output, it also depends on the connectivity matrix
  * User can pass initial individual/full Connectivity matrix to specify the interconnection between subnet.
  *
  * The order of getting connectivity matrix is like: check if there is initial full connectivity matrix, if there is,
  * then just use it. If there is not, then use individual connectivity matrix to generate it. When the individual
  * connectivity matrix is called, we first check whether there is a predefined individual matrix, if there is, then use
  * it. Otherwise we generate one with all diagonal set with true
  *
  * @param initFullMatrix    Initial Full Connectivity Matrix
  * @param initIndividualMat Initial Individual Connectivity Matrix between single port
  */
case class SWRoutingParameters(
  initFullMatrix:    Array[Array[Boolean]] = Array.empty,
  initIndividualMat: Array[Array[Boolean]] = Array.empty)
    extends ReconfParameters
    with JSONParsableParameters {
  // Calculate the default individual connectivity matrix based on the CgraNodeParameter (number of subnet)
  def individualConnMat(implicit p: Parameters): Array[Array[Boolean]] = {
    // Get the number of subnet first
    val numSubnet: Int = p(CompNode).numCompUnit
    // Check if initial individual connectivity matrix is defined already
    if (initIndividualMat.isEmpty) {
      // There is no individual connectivity matrix defined, we should generate our own
      val mat: Array[Array[Boolean]] = Array.fill(numSubnet)(Array.fill(numSubnet)(false))
      // Set the diagonal to be true, which is the normal connection
      for (idx <- 0 until numSubnet) mat(idx)(idx) = true
      // return
      mat
    } else {
      // The individual matrix is defined, first check whether it is consistent with number of subnet
      require(
        initIndividualMat.length == numSubnet,
        s"The number of row (${initIndividualMat.length}) " +
          s"!= number of subnet ($numSubnet)"
      )
      initIndividualMat.zipWithIndex.foreach { case (booleans, i) =>
        require(
          booleans.length == numSubnet,
          s"The number of column (${booleans.length}) of row $i != number of subnet ($numSubnet)"
        )
      }
      // If pass the requirement check, return
      initIndividualMat
    }
  }

  def fullMatSanityCheck(num_input: Int, num_output: Int, numSubnet: Int, sMat: Array[Array[Boolean]]): Unit = {
    // The full connectivity matrix is predefined, we should check whether it satisfies the size
    require(
      sMat.length == num_output * numSubnet,
      s"For a given full connectivity matrix, it has ${sMat.length} rows, which is not equal to" +
        s" numOutput x numSubnet = $num_output x $numSubnet = ${num_output * numSubnet}"
    )
    sMat.zipWithIndex.foreach { case (row, i) =>
      require(
        row.length == num_input * numSubnet,
        s"The number of column of row $i = ${row.length}, " +
          s"which is not equal to numInput x numSubnet = $num_input x $numSubnet = ${num_input * numSubnet}"
      )
    }
    // The given connectivity matrix is legal in size, but we should also check whether each row has at least one input
    sMat.zipWithIndex.foreach { case (row, i) =>
      require(
        row.count(x => x) >= 1,
        s"Row $i (Output ${i / numSubnet} Subnet ${i % numSubnet}) has less than one input"
      )
    }
  }

  // Calculate the full connectivity matrix based on #input/#output and number of subnet
  def fullConnMat(num_input: Int, num_output: Int)(implicit p: Parameters): Array[Array[Boolean]] = {
    // First we should get the number of subnet
    val numSubnet: Int = p(CompNode).numCompUnit
    // Check if we have full connectivity matrix defined already
    if (initFullMatrix.isEmpty) {
      // Full Connectivity Matrix is not defined, we need to generate our own
      // The full connectivity matrix is an numOutput x numSubnet - numInput x numSubnet matrix
      val mat: Array[Array[Boolean]] = Array.fill(num_output * numSubnet)(Array.fill(num_input * numSubnet)(false))
      // Put the individual connectivity matrix in full connectivity matrix
      for (outputIdx <- 0 until num_output) {
        for (inputIdx <- 0 until num_input) {
          for (outputSubnet <- 0 until numSubnet) {
            for (inputSubnet <- 0 until numSubnet) {
              val rowIdx: Int = outputIdx * numSubnet + outputSubnet
              val colIdx: Int = inputIdx * numSubnet + inputSubnet
              val iMat:   Array[Array[Boolean]] = individualConnMat
              mat(rowIdx)(colIdx) = iMat(outputSubnet)(inputSubnet)
            }
          }
        }
      }
      // Return it with sanity check
      fullMatSanityCheck(num_input, num_output, numSubnet, mat)
      mat
    } else {
      // The given connectivity is legal in terms of connectivity per output, so return it
      fullMatSanityCheck(num_input, num_output, numSubnet, initFullMatrix)
      initFullMatrix
    }
  }

  // How many bits required for routing
  def csrBits(num_input: Int, num_output: Int)(implicit p: Parameters): Int =
    csrFieldBits(num_input, num_output).map(_._2).sum

  // Routing Field and CSR Bits
  def csrFieldBits(num_input: Int, num_output: Int)(implicit p: Parameters): Seq[(String, Int)] = {
    fullConnMat(num_input, num_output).zipWithIndex.map { case (row, rowIdx) =>
      val outputIdx:         Int = rowIdx / p(CompNode).numCompUnit
      val subnetIdx:         Int = rowIdx % p(CompNode).numCompUnit
      val numConnectedInput: Int = row.count(x => x)
      // Sanity check: number of connected input should not be zero (I know I did this check many times, by cannot be
      // more careful)
      require(numConnectedInput > 0, s"In Full Connectivity matrix, the $rowIdx row has no input subnet")
      // For each subnet, it must have 1 extra input means the this output does not accept input
      (s"${SwitchRouting.getClass.getSimpleName}_${outputIdx}_SubNet_$subnetIdx", log2Ceil(numConnectedInput + 1))
    }
  }

  implicit val paraWrites: Writes[JSONParsableParameters] = new Writes[SWRoutingParameters] {
    override def writes(o: SWRoutingParameters): JsValue = json.deepMerge(
      Json.obj(
        "initFullMatrix" -> o.initFullMatrix,
        "initIndividualMatrix" -> o.initIndividualMat
      )
    )
  }
}

object SWRoutingParameters extends JSONParsableConstructor {
  override def apply(json: JsValue): SWRoutingParameters =
    SWRoutingParameters(
      initFullMatrix = (json \ "initFullMatrix").as[Array[Array[Boolean]]],
      initIndividualMat = (json \ "initIndividualMatrix").as[Array[Array[Boolean]]]
    )
}
