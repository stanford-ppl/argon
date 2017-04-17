package forge

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox

/**
  * Annotates an entry point from the user's program to the compiler
  * Optionally adds implicit SourceContext parameter if one does not already exist (since all API methods need this)
  */
final class api extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro APIAnnotation.impl
}

/**
  * Annotates entry point from user's program to the compiler, but which shouldn't be documented
  * TODO: May want to make this a string argument on api in the future?
  */
final class util extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro UtilAnnotation.impl
}

final class internal extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro InternalAnnotation.impl
}

object APIAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    FrontendAnnotation.impl(c)(annottees: _*)
  }
}

object UtilAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    FrontendAnnotation.impl(c)(annottees: _*)
  }
}

object InternalAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val withCtx = FrontendAnnotation.impl(c)(annottees: _*)
    withCtx match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val flags = mods.flags | Flag.PROTECTED
        DefDef(Modifiers(flags), name, tparams, vparamss, tpt, rhs)
      case _ => withCtx
    }
  }
}

object FrontendAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val srcCtx = q"ctx: org.virtualized.SourceContext"

    val tree = annottees.head match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val hasImplicits = vparamss.lastOption.exists(_.exists {
          case x: ValDef => x.mods.hasFlag(Flag.IMPLICIT)
        })
        val params = if (hasImplicits) {
          val hasCtx = vparamss.lastOption.exists(_.exists {
            case ValDef(_, _, Ident(TypeName(n)), _) =>
              n == "SrcCtx" || n == "SourceContext" || n == "org.virtualized.SourceContext"
            case _ => false
          })
          if (!hasCtx) {
            vparamss.dropRight(1) :+ (vparamss.lastOption.getOrElse(Nil) ++ List(
              srcCtx))
          } else vparamss
        } else {
          vparamss :+ List(srcCtx)
        }
        q"$mods def $name[..$tparams](...${params.dropRight(1)})(implicit ..${params.last}): $tpt = $rhs"

      case _ =>
        c.abort(c.enclosingPosition, "API annotation can only be used on Def")
    }
    tree
  }
}
