package argon.codegen

import argon.core.compiler._

// Currently unused
trait KernelCodegen extends Codegen {
  def emitKernelHeader(lhs: List[Sym[_]], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean): Unit
  def emitKernelFooter(lhs: List[Sym[_]], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean): Unit
}
