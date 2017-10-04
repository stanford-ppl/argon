package forge

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

final class lazyvar extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro LazyVarAnnotation.impl
}


object LazyVarAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tree = annottees.head match {
      // var name: tpt = rhs
      case ValDef(mods,name,tpt,rhs) if mods.hasFlag(Flag.MUTABLE) =>
        if (tpt == EmptyTree) {
          c.abort(c.enclosingPosition, "@lazyvar requires explicit specification of the type of the var")
        }
        val hiddenVarName = TermName("__"+name)
        val setterName = TermName(name + "_=")

        val hiddenVar = q"var $hiddenVarName: Option[$tpt] = None"
        val methodVar =
          q"""def $name: $tpt = {
                if ($hiddenVarName.isEmpty) $hiddenVarName = Some($rhs)
                $hiddenVarName.get
              }
           """
        val varSetter = q"""def $setterName(rhs: $tpt): Unit = { $hiddenVarName = Some($rhs) }"""

        val res = q"$hiddenVar ; $methodVar ; $varSetter"
        c.info(c.enclosingPosition, showCode(res), true)
        res

      case _ =>
        c.abort(c.enclosingPosition, "@lazyvar annotation can only be used on vars")
    }
    tree
  }
}



