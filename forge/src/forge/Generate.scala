package forge

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

final class generate extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GenerateAnnotation.impl
}

object GenerateAnnotation extends StaticAnnotation {

  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    /*val allRanges = annottees.head.symbol.annotations.flatMap{
      case extr if extr.tree.tpe <:< c.weakTypeOf[generate] =>
        val args = extr.tree.children.tail
        args.head match {
          case Constant(Literal(str: String)) => Some(str)
          case _ => None
        }
      }
    if (allRanges.isEmpty) c.abort(c.enclosingPosition, "Generate requires a set of ranges")

    val ranges = allRanges*/

    val tx = new GenerateTransformer[c.type](c)
    val out = q"..${tx(annottees.head)}"

    /*annottees.head match {
      case x: ValDef    =>
      case x: ClassDef  => q"..${tx(x)}"
      case x: DefDef    => q"..${tx(x)}"
      case x: TypeDef   => q"..${tx(x)}"
      case x =>
        c.warning(c.enclosingPosition, "Generate annotation on the given construct will do nothing: " + showRaw(x))
        x
    }*/

    // c.info(c.enclosingPosition, showCode(out), true)
    out
  }
}
