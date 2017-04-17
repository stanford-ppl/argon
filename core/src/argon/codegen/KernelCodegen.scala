package argon.codegen

trait KernelCodegen extends Codegen {
  import IR._

  def emitKernelHeader(lhs: List[Sym[_]],
                       vals: List[Sym[_]],
                       vars: List[Sym[_]],
                       resultType: String,
                       resultIsVar: Boolean): Unit
  def emitKernelFooter(lhs: List[Sym[_]],
                       vals: List[Sym[_]],
                       vars: List[Sym[_]],
                       resultType: String,
                       resultIsVar: Boolean): Unit
}
