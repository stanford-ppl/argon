package forge

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

/*final class rewritable extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RewritableAnnotation.impl
}

object RewritableAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    annottees.head match {
      case df @ DefDef(mods,name,tparams,paramss,tpt,rhs) =>
        val argTypes = paramss.flatten.map{case ValDef(_,nm,tp,_) => tp }

        q"""
           private var ${TermName(name.toString+"_pf")} = PartialFunction.empty[]}
         """

      case _ =>
        c.abort(c.enclosingPosition, "rewritable annotation can only be used on defs")
    }
  }
}*/
