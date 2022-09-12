package dsagen2.util

import chiseltest.internal.VcsBackendAnnotation
import chiseltest.legacy.backends.vcs.VcsFlags
import firrtl.annotations.NoTargetAnnotation
import treadle.WriteVcdAnnotation

import scala.sys.process._

trait VcsSimulationHelper {
  // Generate VCS Flags based on Linux Distribution
  def vcsFlags: VcsFlags = {
    val description = "lsb_release -d".!!
    if (description.contains("Ubuntu")) {
      // Ubuntu does not have some library which needs these flags
      VcsFlags(Seq("-LDFLAGS -Wl,--no-as-needed"))
    } else {
      VcsFlags(Nil)
    }
  }

  // VCS Annotations
  // If VCS_HOME is set in the system environment variable, it will use VCS as simulation backend
  def vcsHome: Option[String] = sys.env.get("VCS_HOME")

  def vcsAnnotations: Seq[NoTargetAnnotation] =
    vcsHome match {
      // if vcs is accessible, use VCS as simulation backend
      case Some(_) => Seq(VcsBackendAnnotation, vcsFlags)
      case None => Seq(WriteVcdAnnotation)
    }

  // Target Directory Annotations
  def targetDirAnnotation: Seq[NoTargetAnnotation] = Seq.empty

  // Merge Common Annotations
  def VcsDirAnnotations: Seq[NoTargetAnnotation] = vcsAnnotations ++ targetDirAnnotation
}
