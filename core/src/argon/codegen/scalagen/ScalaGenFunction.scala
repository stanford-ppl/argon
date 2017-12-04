package argon.codegen.scalagen

import argon.core._
import argon.nodes._

trait ScalaGenFunction extends ScalaCodegen {
  override protected def remap(tp: Type[_]): String = tp match {
    case FuncType(r) => throw new Exception("Remapping of function types is not yet supported!")
    case _ => super.remap(tp)
  }

  override protected def emitNode(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@FuncDecl(args, block) =>
      val params = args.map{x => src"$x: ${x.tp}" }.mkString(",")
      val name = lhs.name.getOrElse(src"$lhs")
      open(src"def $name($params): ${op.mRet} = {")
        emitBlock(block)
      close("}")

    case FuncCall(func, args) =>
      val name = func.name.getOrElse(src"$func")
      val params = args.map(quote).mkString(",")
      emit(src"val $lhs: ${lhs.tp} = $name($params)")

    case _ => super.emitNode(lhs, rhs)
  }
}
