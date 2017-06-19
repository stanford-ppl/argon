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

final class internal extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro InternalAnnotation.impl
}

final class data extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MetadataAnnotation.impl
}

/**
  * Requires implicit source context, but not necessarily stateful
  */
final class ctxdep extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CtxAnnotation.impl
}

/**
  * Requires implicit compiler state, but not necessarily source context
  */
final class stateful extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro StatefulAnnotation.impl
}

object APIAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    val withCtx = FrontendAnnotation.impl(c)(annottees:_*)
    val withState = StateAnnotation.impl(c)(withCtx)
    withState
  }
}

object InternalAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val withCtx = FrontendAnnotation.impl(c)(annottees:_*)
    val withState = StateAnnotation.impl(c)(withCtx)
    withState
    /*withState match {
      case DefDef(mods,name,tparams,vparamss,tpt,rhs) if !mods.hasFlag(Flag.PRIVATE) && !mods.hasFlag(Flag.PROTECTED) =>
        val flags = mods.flags | Flag.PROTECTED
        DefDef(Modifiers(flags),name,tparams,vparamss,tpt,rhs)
      case _ => withState
    }*/
  }
}

object MetadataAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    annottees.head match {
      case ModuleDef(_,_,_) => StateAnnotation.impl(c)(annottees:_*)
      case _ => c.abort(c.enclosingPosition, "@metadata annotation can only be used on objects")
    }
  }
}


object CtxAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    FrontendAnnotation.impl(c)(annottees:_*)
  }
}

object StatefulAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    StateAnnotation.impl(c)(annottees:_*)
  }
}


object FrontendAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val srcCtx = q"ctx: org.virtualized.SourceContext"

    val tree = annottees.head match {
      case DefDef(mods,name,tparams,vparamss,tpt,rhs) =>
        val hasImplicits = vparamss.lastOption.exists(_.exists{
          case x: ValDef => x.mods.hasFlag(Flag.IMPLICIT)
        })
        val params = if (hasImplicits) {
          val hasCtx = vparamss.lastOption.exists(_.exists{
            case ValDef(_,_,Ident(TypeName(n)),_) => n == "SrcCtx" || n == "SourceContext"
            case ValDef(_,_,Select(Select(Ident(TermName("org")), TermName("virtualized")), TypeName("SourceContext")), EmptyTree) => true
            case _ => false
          })
          if (!hasCtx) {
            vparamss.dropRight(1) :+ (vparamss.lastOption.getOrElse(Nil) ++ List(srcCtx))
          }
          else vparamss
        }
        else {
          vparamss :+ List(srcCtx)
        }
        q"$mods def $name[..$tparams](...${params.dropRight(1)})(implicit ..${params.last}): $tpt = $rhs"

      case _ =>
        c.abort(c.enclosingPosition, "API annotation can only be used on Def")
    }
    tree
  }
}

object StateAnnotation {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val state = q"state: argon.core.State"

    val tree = annottees.head match {
      case DefDef(mods,name,tparams,vparamss,tpt,rhs) =>
        val hasImplicits = vparamss.lastOption.exists(_.exists{
          case x: ValDef => x.mods.hasFlag(Flag.IMPLICIT)
        })
        val params = if (hasImplicits) {
          val hasCtx = vparamss.lastOption.exists(_.exists{
            case ValDef(_,_,Ident(TypeName("State")),_) => true
            case ValDef(_,_,Select(Select(Ident(TermName("argon")), TermName("core")), TypeName("State")), EmptyTree) => true
            case _ => false
          })
          if (!hasCtx) {
            vparamss.dropRight(1) :+ (vparamss.lastOption.getOrElse(Nil) ++ List(state))
          }
          else vparamss
        }
        else {
          vparamss :+ List(state)
        }
        q"$mods def $name[..$tparams](...${params.dropRight(1)})(implicit ..${params.last}): $tpt = $rhs"


      case ModuleDef(mods,name,Template(parents, selfType, bodyList)) =>
        val (fields, methods) = bodyList.partition { case _:ValDef => true case _ => false }
        val (constructors, defs) = bodyList.partition{ case DefDef(_,defName,_,_,_,_) => defName == termNames.CONSTRUCTOR }

        val defs2 = defs.map{method => StateAnnotation.impl(c)(method) }

        ModuleDef(mods, name, Template(parents, selfType, fields ++ constructors ++ defs2))

      case _ =>
        c.abort(c.enclosingPosition, "@stateful annotation can only be used on objects and defs")
    }
    tree
  }
}


