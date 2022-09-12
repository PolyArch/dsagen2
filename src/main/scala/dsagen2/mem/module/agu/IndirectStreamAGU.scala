package dsagen2.mem.module.agu

import chisel3._
import chisel3.util._
import dsagen2.mem.config.MemNodeParameters
import dsagen2.top.diplomacy.DSANodeType._
import dsagen2.util.StreamUtil.{actualValueOfSignUInt, singelElemRowMask}

object IndirectStreamAGU {

  /** Indirect Address Generation
    *
    * @param startAddr      Start Address
    * @param stride1D       Stride 1D
    * @param memDataTypeExp Memory Data Type
    * @param idxValues      Index value in byte, whose length = memWidth
    * @param idxValids      Index value valid
    * @param idxDataTypeExp Index Data Type
    * @param memNode        Memory Parameters
    * @return (
    *         #requestPort x Address,
    *         #requestPort x Bitmask,
    *         #requestPort x Valid,
    *         number of accessed element
    *         )
    */
  def IndirectAddrGen(
    startAddr:      UInt,
    stride1D:       Option[UInt],
    memDataTypeExp: Option[UInt],
    idxValues:      Vec[UInt],
    idxValids:      Vec[Bool],
    idxDataTypeExp: Option[UInt],
    memNode:        MemNodeParameters
  ): (Vec[UInt], Vec[UInt], Vec[Bool], UInt) = {
    // Sanity Check: must support Indirect Stream
    require(memNode.IndirectIndexStream, s"If indirect index is not supported, why you call this function")

    // Create result wires
    val vecAddr: Vec[UInt] = WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(0.U(startAddr.getWidth.W))))
    val vecMask: Vec[UInt] = WireInit(
      VecInit(
        Seq.fill(memNode.numMemReqPort)(
          0.U(
            {
              memNode.nodeType match {
                case DirectMemoryAccess => memNode.bandwidth.W
                case ScratchpadMemory   => memNode.spmBankWidth.W
                case GenerateEngine     => memNode.bandwidth.W
                case _                  => require(requirement = false, s"Node Type ${memNode.nodeType} is wrong"); 1.W
              }
            }
          )
        )
      )
    )
    val vecValid:  Vec[Bool] = WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(false.B)))
    val numAccess: UInt = PopCount(vecValid)

    // Create Index valid under every data type, if under a data type, the number of group formed is less than
    // request port, it means that some higher request ports will never be valid
    // reqIdxValid[dataTypeExp][reqIdx]
    val reqIdxValid: Seq[Vec[Bool]] = for (idxDataTypeExp <- 0 until memNode.NumIdxDataTypeExp) yield {
      // Create Index Valid for each request port
      val reqIdxValid: Vec[Bool] = WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(false.B)))
      // Calculate the number of valid this data type will group
      val numGroupValid: Int = 1 << idxDataTypeExp
      // Group Valid and AND reduce them
      val groupIdxValid: Seq[Bool] = idxValids
        .grouped(numGroupValid)
        .zipWithIndex
        .map { case (bools, i) =>
          // Data must be valid if all data unit are valid
          if (i < memNode.numMemReqPort) VecInit(bools).asUInt().andR()
          else false.B
        }
        .toSeq
      // Connect the first #numMemReqPort group
      for (reqIdx <- 0 until memNode.numMemReqPort) {
        if (reqIdx < groupIdxValid.length) {
          reqIdxValid(reqIdx) := groupIdxValid(reqIdx)
        } else {
          reqIdxValid(reqIdx) := false.B
        }
      }
      // Return
      reqIdxValid
    }

    // Create Index Value under every data type
    // reqIdxValue[dataTypeExp][reqIdx]
    val reqIdxValue: Seq[Vec[UInt]] = for (idxDataTypeExp <- 0 until memNode.NumIdxDataTypeExp) yield {
      // Create Index Value for each request port
      val reqIdxValue: Vec[UInt] = WireInit(VecInit(Seq.fill(memNode.numMemReqPort)(0.U(startAddr.getWidth.W))))
      // Calculate the number of byte that this data type will group
      val numGroupValue: Int = 1 << idxDataTypeExp
      // Group value and concat them
      val groupIdxValue: Seq[UInt] = idxValues
        .grouped(numGroupValue)
        .zipWithIndex
        .map { case (ints, i) =>
          if (i < memNode.numMemReqPort) VecInit(ints).asUInt()
          else 0.U
        }
        .toSeq
      // connect the first #numMemReqPort
      for (reqIdx <- 0 until memNode.numMemReqPort) {
        if (reqIdx < groupIdxValue.length) {
          reqIdxValue(reqIdx) := groupIdxValue(reqIdx)
        } else {
          reqIdxValue(reqIdx) := 0.U
        }
      }
      // Return
      reqIdxValue
    }

    // Connect to each request address and valid, generate mask
    for (reqIdx <- 0 until memNode.numMemReqPort) {
      // Get index value of this request idx
      val reqIndexLUT: Seq[(UInt, UInt)] =
        reqIdxValue.map(_.apply(reqIdx)).zipWithIndex.map { case (index, i) => i.U -> index }
      val reqValidLUT: Seq[(UInt, Bool)] =
        reqIdxValid.map(_.apply(reqIdx)).zipWithIndex.map { case (valid, i) => i.U -> valid }
      // Get index and valid of this index data type
      val indexValue: UInt = MuxLookup(idxDataTypeExp.getOrElse(0.U), 0.U, reqIndexLUT)
      val indexValid: Bool = MuxLookup(idxDataTypeExp.getOrElse(0.U), false.B, reqValidLUT)
      // Calculate address
      val s1DProd: SInt = actualValueOfSignUInt(startAddr.getWidth, stride1D).asSInt() * reqIdx.S
      val delta:   UInt = WireInit(0.U(startAddr.getWidth.W))
      delta := s1DProd.asUInt()
      val indAddr: UInt = WireInit(0.U(startAddr.getWidth.W))
      // This is where indirect address actually be calculated
      indAddr := startAddr + ((indexValue + delta) << memDataTypeExp.getOrElse(0.U))
      // Connect
      vecAddr(reqIdx) := indAddr
      vecMask(reqIdx) :=
        Mux(
          indexValid,
          singelElemRowMask(indAddr, memDataTypeExp, memNode.numMemDataTypeExp, vecMask.head.getWidth),
          0.U
        )
      vecValid(reqIdx) := indexValid
    }

    // Return
    (vecAddr, vecMask, vecValid, numAccess)
  }
}
