package dsagen2.util
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.DataMirror
import chisel3.internal.requireIsChiselType
import chisel3.stage.ChiselStage
import dsagen2.comp.impl.FunctionUnitModule
import dsagen2.top.config.operation.OperDataType.DsaOperDataType
import logger.{LogLevel, LogLevelAnnotation}
import org.reflections.Reflections

import java.io.{File, FileWriter}
import scala.collection.JavaConverters._
import scala.collection.Set
import scala.sys.process._

object AppUtil {
  // delete generated output
  def deleteGenOutput(designName : String) : Unit = {
    s"rm $designName.v $designName.fir $designName.anno.json".!
  }

  // Write string to certain path with filename
  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val directory = new File(targetDir)
    if (!directory.exists) {
      directory.mkdir
    }
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close()
    f
  }

  /**
   * Get all objects of certain trait
   * @param pkgPath the package name of trait
   * @param underlying classOf(trait)
   * @tparam T trait type
   * @return all object that implement the trait under that package
   */
  def subObjects[T](pkgPath : String, underlying: Class[T]): Seq[T] = {
    val reflects = new Reflections(pkgPath)
    reflects.getSubTypesOf(underlying).asScala.map { sub =>
      sub.getField("MODULE$").get(null).asInstanceOf[T]
    }.toSeq
  }

  // Emit Verilog for Function Unit Module
  def emitFuVerilog(opDataTypeSet : Set[DsaOperDataType],
                    dataPathBits : Int)(implicit p : Parameters) : Unit = {
    // Assign encoding
    val encoding = opDataTypeSet.toSeq.zipWithIndex.toMap
    // Emit Verilog
    (new ChiselStage).emitVerilog(
      new FunctionUnitModule(encoding.keys.toSet, encoding, dataPathBits, encoding.size),
      Array("--full-stacktrace", "--target-dir", "vsrc/fu"),
      Seq(LogLevelAnnotation(LogLevel.Error))
    )
  }

  /**
   * Convert chisel/hardware type to chisel type
   * @param gen chisel/hardware type
   * @tparam T Type of it
   * @return chisel type
   */
  def gen2genType[T <: Data](gen : T, comOpt : CompileOptions) : T = {
    val genType : T = if (comOpt.declaredTypeMustBeUnbound) {
      requireIsChiselType(gen)
      gen
    } else {
      if (DataMirror.internal.isSynthesizable(gen)) {
        chiselTypeOf(gen)
      } else {
        gen
      }
    }
    genType
  }

  /**
   * bitvec can only be passed with synchronized position is true
   * @param bitvec Input bitvector
   * @param optionSync optional synchronization signal
   * @return Synchronized bit vector
   */
  def bitvecSynchronized(bitvec : Seq[Bool], optionSync : Option[Seq[Bool]]) : Seq[Bool] = {
    // Switch
    optionSync match {
      case Some(sync) =>
        // True exist
        val exist : Bool = VecInit(bitvec).asUInt().orR()
        val syncExist : Bool = VecInit(sync).asUInt().orR()
        // All sync true
        val allSyncTrue : Bool = VecInit(bitvec.zip(sync).map{case (valid, sync) =>
          Mux(sync, valid, true.B)
        }).asUInt().andR()
        // Return
        val result : UInt =
          Mux(syncExist, Mux(exist && allSyncTrue, VecInit(bitvec).asUInt(), 0.U), VecInit(bitvec).asUInt())
        result.asBools()
      // If synchronization is not supported, then just pass through the bit vector
      case None => bitvec
    }
  }

  /**
   * Add one increment
   * @param start Starting point at lowest
   * @param bitvec bit vector to indicate whether increase by 1 or not
   * @return increased by 1 at true position
   * start = 2, bitvec = [0, 1, 1, 0, 1, 1, 1, 0] ==> [6, 6, 5, 4, 4, 3, 2, 2]
   */
  def addOneIncrement(start : UInt, bitvec : Seq[Bool]) : Seq[UInt] = {
    val rawResult = bitvec.scanLeft(start - 1.U)((prev, valid) => Mux(valid, prev + 1.U, prev)).drop(1)
    require(rawResult.length == bitvec.length)
    if(start.widthKnown){
      rawResult.map(x => x.apply(start.getWidth - 1, 0))
    }else{
      rawResult
    }
  }
}
