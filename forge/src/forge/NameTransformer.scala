package forge

import language.experimental.macros
import scala.reflect.macros.blackbox

class NameTransformer[Ctx <: blackbox.Context](val c: Ctx) {
  import c.universe._

  private val transformer = new Tx(Map.empty)

  def apply(x: TypeName, i: (String, Any)*): TypeName = {
    transformer.subst = i.toMap
    transformer.apply(x)
  }
  def apply(x: TermName, i: (String, Any)*): TermName = {
    transformer.subst = i.toMap
    transformer.apply(x)
  }
  def apply[T <: Tree](x: T, i: (String, Any)*): T = {
    transformer.subst = i.toMap
    transformer.apply(x)
  }
  def apply[T <: Tree](xs: List[T], i: (String, Any)*): List[T] = {
    transformer.subst = i.toMap
    transformer.apply(xs)
  }
  def fs[T <: Tree](xss: List[List[T]], i: (String, Any)*): List[List[T]] = {
    transformer.subst = i.toMap
    transformer.fs(xss)
  }

  private class Tx(var subst: Map[String, Any]) extends Transformer {
    def f = this

    def apply(x: String): String = {
      //c.info(c.enclosingPosition, s"Replacing in $x: $subst", true)
      val result = subst.foldRight(x) {
        case ((orig, repl), cur) => cur.replaceAll(orig, repl.toString)
      }
      //c.info(c.enclosingPosition, s"Created $result", true)
      result
    }

    def apply(x: TypeName): TypeName = x match {
      case TypeName(x) => TypeName(f(x))
    }
    def apply(x: TermName): TermName = x match {
      case TermName(x) => TermName(f(x))
    }
    def apply(x: Name): Name = x match {
      case x: TypeName => f(x)
      case x: TermName => f(x)
    }
    def apply[T <: Tree](x: T): T = transform(x).asInstanceOf[T]
    def apply[T <: Tree](xs: List[T]): List[T] = xs.map { x =>
      f(x)
    }
    def fs[T <: Tree](xss: List[List[T]]): List[List[T]] = xss.map { xs =>
      f(xs)
    }

    override def transform(tree: Tree): Tree = atPos(tree.pos) {
      tree match {
        case ClassDef(mods, name, parents, template) =>
          ClassDef(mods, f(name), f(parents), f(template))
        case TypeDef(mods, name, tpars, rhs) =>
          TypeDef(mods, f(name), f(tpars), f(rhs))
        case DefDef(mods, name, tpars, vparamss, tp, body) =>
          DefDef(mods, f(name), f(tpars), fs(vparamss), f(tp), f(body))
        case ValDef(mods, name, tp, rhs) =>
          ValDef(mods, f(name), f(tp), f(rhs))
        case Select(qualifier, name) => Select(f(qualifier), f(name))
        case Ident(TermName(x)) if subst.contains(x) =>
          Literal(Constant(subst(x)))
        case Ident(x: TermName) => Ident(f(x))
        case Ident(x: TypeName) => Ident(f(x))
        case _                  => super.transform(tree)
      }
    }
  }

}
