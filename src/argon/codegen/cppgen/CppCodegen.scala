package argon.codegen.cppgen

import argon.codegen.Codegen
import argon.Config
import argon.codegen.FileDependencies
import sys.process._
import scala.language.postfixOps



trait CppCodegen extends Codegen with FileDependencies  {
  import IR._
  override val name = "Cpp Codegen"
  override val lang: String = "cpp"
  override val ext: String = "cpp"

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"// results in ${b.result}")
  }

  final protected def emitController(b: Block[_]): Unit = {
    visitBlock(b)
    emit(src"// ctrl results in ${b.result}")
  }

  override def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case lhs: Sym[_] => s"x${lhs.id}"
  }

  override def copyDependencies(out: String): Unit = {
    val cppResourcesPath = s"${sys.env("SPATIAL_HOME")}/src/spatial/codegen/cppgen/resources"

    // FIXME: Should be OS-independent. Ideally want something that also supports wildcards, maybe recursive copy
    s"""rm -rf ${out}/datastructures""".!

    s"""cp -r $cppResourcesPath/datastructures ${out}""".!
    s"""cp -r $cppResourcesPath/fringeSW ${out}""".!

    s"""mv ${out}/cpptypes.h ${out}/datastructures""".!
    s"""mv ${out}/interface.h ${out}/datastructures""".!
    s"""mv ${out}/DRAM.h ${out}/datastructures""".!
    super.copyDependencies(out)
  }





}
