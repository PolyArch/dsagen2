package dsagen2.comp.impl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import dsagen2.comp.bundle.CompDataBundle
import dsagen2.comp.config.CompKeys.SwitchRouting
import dsagen2.comp.config.CompNodeParameters
import dsagen2.comp.config.switch.SWRoutingParameters

/** The capability of routing numInput to numOutput, while keeping the dataType unchanged
  */
trait RoutingModuleImpl {
  /* Parameters */
  implicit val p:     Parameters
  val routeNumInput:  Int
  val routeNumOutput: Int
  val compNode:       CompNodeParameters

  /* Virtual Floating Wire */
  val routeInputs:     Seq[DecoupledIO[CompDataBundle]]
  val routeOutputs:    Seq[DecoupledIO[CompDataBundle]]
  val routeSelects:    Seq[UInt]
  val outputHasSource: Seq[Bool]
  val inputHasSink:    Seq[Bool]
  val inputReadies:    Seq[Bool]

  // Number of Subnet
  def numSubnet: Int = compNode.numCompUnit

  // Flatten the vector of data
  def flattenTagValue(isInput: Boolean): Seq[UInt] =
    if (isInput) {
      // Flatten the input's tag value
      val inputFlatten: Seq[UInt] = routeInputs.flatMap(_.bits.vecData.toSeq)
      /*
            require(inputFlatten.length == 24) //TODO, this is adhoc check
       */
      // Sanity check: make sure that flatten input length is correct
      require(
        inputFlatten.length == routeNumInput * numSubnet,
        s"The flatten input's length (${inputFlatten.length}) is not equal to expected: " +
          s"numInput x numSubnet = $routeNumInput x $numSubnet"
      )
      // Return
      inputFlatten
    } else {
      // Flatten output's data
      val outputFlatten: Seq[UInt] = routeOutputs.flatMap(_.bits.vecData.toSeq)
      // Sanity Check: make sure the flatten output length is correct
      require(
        outputFlatten.length == routeNumOutput * numSubnet,
        s"The flatten output's length (${outputFlatten.length}) is not equal to expected: " +
          s"numOutput x numSubnet = $routeNumInput x $numSubnet"
      )
      // Return
      outputFlatten
    }

  // Get subnets among inputs by using mask
  def maskInputTagValueValid(mask: Array[Boolean]): (Seq[UInt], Seq[Bool]) = {
    // First we need to extract the bits part of DataBundle
    val inputValids: Seq[Bool] = routeInputs.map(_.valid)
    // Flatten the vector of Tagged Value
    val vecData: Seq[UInt] = flattenTagValue(isInput = true)
    // Duplicate the valid for numSubnet time
    require(inputValids.length == inputReadies.length)
    val vecValid: Seq[Bool] = inputValids.zip(inputReadies).flatMap { case (b, r) => Fill(numSubnet, b && r).asBools() }
    // Zip with mask and filter to get the target data
    val muxCandidate: Seq[UInt] = vecData.zip(mask).filter(_._2).map(_._1)
    // Zip with mask and filter to get the target valid
    val muxValid: Seq[Bool] = vecValid.zip(mask).filter(_._2).map(_._1)
    // There must be more than one candidate otherwise it will be illegal
    require(muxCandidate.nonEmpty, s"One subnet of one output port get no inputs")
    // Vector valid and TagValue should have the same length
    require(
      vecData.length == vecValid.length,
      s"The length of flatten vector of TagValue (${vecData.length}) " +
        s" and the length of filled flatten vector of valid (${vecValid.length}) should be same"
    )
    require(muxValid.length == muxCandidate.length)
    // Return
    (muxCandidate, muxValid)
  }

  // Connect Routing Wire
  def route(): Unit = {
    // Get the routing parameters first
    val routeParam: SWRoutingParameters = p(SwitchRouting)

    // Get the full connectivity matrix with sanity check
    val fullConnMat: Array[Array[Boolean]] = routeParam.fullConnMat(routeNumInput, routeNumOutput)

    // Sanity Check: The length of input and output decoupled IO should be equal to numInput/nunOutput
    require(
      routeInputs.length == routeNumInput,
      s"Length of To be routed inputs (${routeInputs.length}) does not " +
        s"equal to the number of input ($routeNumInput)"
    )
    require(
      routeOutputs.length == routeNumOutput,
      s"Length of To be routed outputs (${routeOutputs.length}) does not " +
        s"equal to the number of output ($routeNumInput)"
    )

    // Sanity check: The bitwidth of DataType should be multiple of number of subnet
    routeInputs.foreach { input =>
      require(
        input.bits.getWidth % numSubnet == 0,
        s"The bitwidth of input is ${input.bits.getWidth}, which is not the multiple of" +
          s" the number of subnet is $numSubnet"
      )
    }
    routeOutputs.foreach { output =>
      require(
        output.bits.getWidth % numSubnet == 0,
        s"The bitwidth of output is ${output.bits.getWidth}, which is not the multiple of" +
          s" the number of subnet is $numSubnet"
      )
    }

    // Sanity Check: Since we control the output routing as per subnet, the number of select should be equal to
    // num_output * numSubnet
    require(
      routeSelects.length == routeNumOutput * numSubnet,
      s"Switch has $routeNumOutput output port and $numSubnet subnet per output port, but only has" +
        s"${routeSelects.length} selects in total"
    )

    // Downward Connection
    routeOutputs.zipWithIndex.foreach { case (output: DecoupledIO[CompDataBundle], outputIdx: Int) =>
      // Loop over all subnets to form the vector of TagValue for this output port
      val muxVecDataValid: Seq[(UInt, Bool, Seq[Bool])] =
        for (subnetIdx <- 0 until numSubnet) yield {
          // Get the route select wire for this subnet of this output port
          val sel: UInt = routeSelects(outputIdx * numSubnet + subnetIdx)
          sel.suggestName(s"route_output${outputIdx}_subnet$subnetIdx")
          // Get the target row of connectivity matrix
          val mask: Array[Boolean] = fullConnMat(outputIdx * numSubnet + subnetIdx)
          // Get the MUX candidate by mux
          val muxCandidate: Seq[UInt] = maskInputTagValueValid(mask)._1
          val muxValid:     Seq[Bool] = maskInputTagValueValid(mask)._2
          // Check whether the number of MUX candidate and the bit width of select line is consistent
          if (muxCandidate.length > 1) {
            require(
              log2Ceil(muxCandidate.length) <= sel.getWidth,
              s"There are ${muxCandidate.length} MUX candidate inputs, but the select line is only" +
                s"${sel.getWidth}-bit wide"
            )
          } else {
            // For this subnet, there is only one input, so there is no need for a select line
            require(
              sel.getWidth == 1,
              s"There is only one candidate input for output $outputIdx subnet $subnetIdx, so the select " +
                s"line should be just 0.U(1.W) wire"
            )
          }
          // Generate the selection one hot for bits and valid
          require(
            muxCandidate.length == muxValid.length,
            s"Bits and Valid has different length: " +
              s"${muxCandidate.length} != ${muxValid.length}"
          )
          // Start from 1 since 0 select ground
          val sel1H: Seq[Bool] = (1 to muxCandidate.length).map(x => x.U === sel)
          // Mux 1H
          (Mux1H(sel1H, muxCandidate), Mux1H(sel1H, muxValid), sel1H)
        }
      // Extract the sequence of TagValue from Mux output, to be form vector of TagValue
      val muxVecTagValue: Seq[UInt] = muxVecDataValid.map(_._1)
      // For this output port, only the valid bit from the input port of each subnet are all valid,
      // the output port can be valid
      val muxValid: Bool = VecInit(muxVecDataValid.map(_._2)).asUInt().andR()
      // For this output port, the hasSource signal mean all subnet has source
      // The first orR is applied to 1H, the second andR is applied to all subnet
      val hasSource: Bool = VecInit(muxVecDataValid.map(x => VecInit(x._3).asUInt().orR())).asUInt().andR()
      // Connection
      output.valid := muxValid // Valid
      output.bits.vecData.zip(muxVecTagValue).foreach { case (outData, muxData) => outData := muxData }
      outputHasSource(outputIdx) := hasSource
    }

    // Upward Connection for ready
    require(routeInputs.length == inputReadies.length)
    routeInputs.zip(inputReadies).zipWithIndex.foreach { case ((input, inputReady), inputIdx) =>
      // For the current input port, we should first loop over all its subnets, collect all ready for each subnet
      val subnetReadyies: Seq[(Bool, Bool)] = for (subnetIdx <- 0 until numSubnet) yield {
        // Calculate the column index of this input subnet
        val inputSubnetIdx: Int = inputIdx * numSubnet + subnetIdx
        // First we should collect the index of all output subnet that can connect to this input subnet
        val connectedOutputSubnetIdx: Seq[Int] = fullConnMat.zipWithIndex.filter { case (rows, _) =>
          // If the current output subnet is connected to this input subnet, it will be true
          rows(inputSubnetIdx)
        }.map(_._2) // Second element is the index of the row, which is the output subnet index
        // Sanity Check: all input subnet should have at least one output subnet connected to it
        require(connectedOutputSubnetIdx.nonEmpty, s"Input Port $inputIdx, Subnet $subnetIdx has no connected output")
        // For all these index of connect output subnets, find the value when they select this input subnet
        val selValues: Seq[Int] = connectedOutputSubnetIdx.map { outputSubnetIdx =>
          // Get the connectivity row for this output subnet
          val row: Array[Boolean] = fullConnMat(outputSubnetIdx)
          // Sanity Check: make sure that this input subnet can be connect to this output subnet
          require(
            row(inputSubnetIdx),
            "This is weird, because this output subnet is filtered since it can be " +
              "connected to this input subnet, but now it cannot, why?"
          )
          // Find out the select value for this input subnet, count TRUE before this input subnet
          val connBefore: Array[Boolean] = row.slice(0, inputSubnetIdx)
          // Count the number of TRUE, which will be the select value for this input subnet
          val selValue: Int = connBefore.count(x => x)
          // Return
          // so if there is no other input subnet connected to this output subnet, the count of true will be 0
          // but since for that output subnet, sel = 1 means select this input subnet, so we should + 1
          selValue + 1 // + 1 is because when an output subnet sel is 0, it connect to nowhere
        }
        // For all these index of connected output subnet, get the routing select for them
        val routeSel: Seq[UInt] = connectedOutputSubnetIdx.map(routeSelects(_))
        // For all these index of connected output subnet, get the ready signal of them
        val readies: Seq[Bool] = connectedOutputSubnetIdx.map { outputSubnetIdx =>
          // Get the output port number first
          val outputIdx: Int = outputSubnetIdx / numSubnet
          // Get the ready signal
          routeOutputs(outputIdx).ready
        }
        // Zip select value, select line, ready together and create the ready signal for this input subnet
        val (outputSubnetReadies, outputSubnetSelecteds): (Seq[Bool], Seq[Bool]) =
          selValues
            .zip(routeSel.zip(readies))
            .map { case (selVal, (sel, r)) =>
              // The reason of "not selected as true" is because we need to AND reduce all ready signals,
              // if "not selected as false", then we cannot do AND reduce
              val selected: Bool = sel === selVal.U
              (Mux(selected, r, true.B), selected)
            }
            .unzip
        // Chances are that none of the connected output subnet select this input subnet, in this case, we should
        // pull down the ready signal, so we calculate that as precondition to use readies
        val atLeastOneSelected: Bool = VecInit(outputSubnetSelecteds).asUInt().orR()
        // Calculate the selected ready wire are all ready, not selected are treated as ready
        val allReady: Bool = VecInit(outputSubnetReadies).asUInt().andR()
        // Return
        (atLeastOneSelected, allReady)
      }
      // All subnet has sink
      val hasSink: Bool = VecInit(subnetReadyies.map(_._1)).asUInt().andR()
      // All subnet are ready
      val allSubnetReady: Bool = VecInit(subnetReadyies.map(_._2)).asUInt().andR()
      // Assign to the ready signal of this input port, this input port are ready is all subnet are ready
      inputReady := hasSink && allSubnetReady
      input.ready := inputReady
      // Input has sink
      inputHasSink(inputIdx) := hasSink
    }
  }

  // Some output subnet may only have one input subnet, so there is no bits to encode it. for the implementation
  // consistency, we should complete the mixed vector of output subnet selects.
  def CompleteOutputSubnetSelect(rawOutputSelect: Option[MixedVec[UInt]]): Seq[UInt] = {
    val result = rawOutputSelect match {
      case Some(o) =>
        for (outputIdx <- 0 until routeNumOutput) {
          for (subIdx <- 0 until numSubnet) {
            o(outputIdx * numSubnet + subIdx).suggestName(s"route_output${outputIdx}_subnet$subIdx")
          }
        }
        o
      case None =>
        require(routeNumInput == 1, s"You have more than one inputs, but you don't need output routing config?")
        Seq.fill(routeNumOutput * numSubnet)(WireInit(0.U)) // no output routing should means only one input port
    }
    require(result.length == routeNumOutput * numSubnet, s"Output Routing Control configuration is problematic")
    result
  }
}
