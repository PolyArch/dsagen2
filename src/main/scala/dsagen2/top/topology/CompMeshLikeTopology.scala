package dsagen2.top.topology

import chipsalliance.rocketchip.config.Parameters
import chisel3.internal.sourceinfo.SourceInfo
import dsagen2.comp.config.CompKeys.CompNode
import dsagen2.comp.module.CompNodeModule

trait CompMeshLikeTopology {
  def createCompNode(config: Parameters)(implicit p: Parameters): CompNodeModule

  /* ---------- Topology Building Utility ---------- */

  /** Create a compute node mesh
    *
    * @param nodeCfg Node configuration
    * @param row     Number of Row
    * @param col     Number of Column
    * @param p       CDE Parameters
    * @return Created Compute Node Mesh
    */
  def createCompNodeMesh(
    nodeCfg: Parameters,
    row:     Int,
    col:     Int
  )(
    implicit p: Parameters
  ): Array[Array[CompNodeModule]] = {
    val noteMesh: Array[Array[String]] =
      (0 until row).map(r => (0 until col).map(c => s"row${r}_col$c").toArray).toArray
    noteMesh.map { rows =>
      rows.map { comment =>
        val copyParam: Parameters = nodeCfg.alterPartial({ case CompNode => nodeCfg(CompNode).copy(comment = comment) })
        createCompNode(copyParam)
      }
    }
  }

  /** {{{
    *   SW ←───→ SW
    *   ↑ \→   ←/↑
    *   │   PE   │
    *   ↓ /→  ←\→↓
    *   SW ←───→ SW
    * }}}
    *
    * @param swMesh Switch Mesh
    * @param peMesh Processing Element Mesh
    * @return
    */
  def Switch4_PE1_MeshTopology(
    swMesh: Array[Array[CompNodeModule]],
    peMesh: Array[Array[CompNodeModule]]
  )(
    implicit p: Parameters,
    sourceInfo: SourceInfo
  ): Unit = {
    val peRow: Int = peMesh.length
    val peCol: Int = peMesh.head.length;
    require(peMesh.forall(x => x.length == peCol), s"PE mesh is not rectangle")
    val swRow: Int = swMesh.length
    val swCol: Int = swMesh.head.length;
    require(swMesh.forall(x => x.length == swCol), s"SW mesh is not rectangle")
    require(swRow == peRow + 1 && swCol == peCol + 1, s"Switch mesh should be 1 larger than PE mesh")

    // Connect over all cells
    for {
      row_idx <- 0 until peRow
      col_idx <- 0 until peCol
    } {
      val upperLeftSw:  CompNodeModule = swMesh(row_idx)(col_idx)
      val upperRightSw: CompNodeModule = swMesh(row_idx)(col_idx + 1)
      val lowerLeftSw:  CompNodeModule = swMesh(row_idx + 1)(col_idx)
      val lowerRightSw: CompNodeModule = swMesh(row_idx + 1)(col_idx + 1)
      val pe:           CompNodeModule = peMesh(row_idx)(col_idx)

      // First Row and First Col
      upperRightSw <-> upperLeftSw
      lowerLeftSw <-> upperLeftSw

      // 4 Input, 1 Output
      upperLeftSw --> pe
      upperRightSw --> pe
      lowerLeftSw --> pe
      lowerRightSw <-> pe

      // Last Row and Last Col
      if (row_idx == peRow - 1) lowerRightSw <-> lowerLeftSw
      if (col_idx == peCol - 1) lowerRightSw <-> upperRightSw
    }
  }

}
