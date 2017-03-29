package macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox //TODO: should be whitebox?


/** Annotation class for @struct macro annotation. */
final class struct extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro StagedStructsMacro.impl
}

/**
  * Towards modularity:
  * Defining and composing macros in different DSLs
  *
  * Want possible type classes for struct to be generated here, based on the type classes defined/available in the DSL
  * This should eventually be generated from Forge (?), for now just outlining how it might work.
  */
abstract class TypeclassMacro {
  def generateLookup(c: blackbox.Context)(name: c.TypeName): Option[c.Tree]
  def generateImplementation(c: blackbox.Context)(className: c.Tree): List[c.Tree]
}


object StagedStructsMacro {
  val typeclasses: List[TypeclassMacro] = List(Bits, Ariths)

  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    if (annottees.length > 1)
      c.abort(c.enclosingPosition, "Only classes can be transformed using the @struct annotation")

    val tree = annottees.head
    tree match {
      // look for class definition
      case ClassDef(mods, className: TypeName, tparams, impl@Template(parents, selfType, bodyList)) =>
        val (fields, methods) = bodyList.partition { case _:ValDef => true case _ => false }
        if (fields.isEmpty)
          c.abort(c.enclosingPosition, "Classes need at least one field in order to be transformed into structs")
        if (methods.size > 1)
          c.abort(c.enclosingPosition, "Classes with a body (e.g. methods) cannot be transformed into structs")
        if (tparams.size > 0)
          c.abort(c.enclosingPosition, "Classes with type parameters cannot be transformed into structs")

        assert(methods.head match { case _: DefDef => true }) // the constructor

        /**
          * Staged class definition
          */
        val fieldList = fields map {
          case ValDef(mods, termName, typeIdent, rhs) if mods.hasFlag(Flag.MUTABLE) =>
            //q"var $termName: $typeIdent"
            c.abort(c.enclosingPosition, "virtualization of variable fields is currently unsupported")
          case ValDef(mods, termName, typeIdent, rhs) =>
            q"""def $termName: $typeIdent = field[$typeIdent](${Literal(Constant(termName.toString))})"""
        }

        val cls =
          q"""
            case class $className(s: Exp[$className]) extends MetaStruct[$className] {
              ..$fieldList
            }
           """

        /**
          * Typeclass evidences and lookup traits
          */
        val evidences = typeclasses.flatMap{_.generateImplementation(c)(tree) }

        /**
          * Staged type
          */
        val childList = fields.map{
          case ValDef(_, termName, typeIdent, rhs) =>
            q""" ${Literal(Constant(termName.toString))} -> typ[$typeIdent]"""
        }

        val lookups = typeclasses.flatMap(_.generateLookup(c)(className) )
        val parent = {
          if (lookups.length > 1) CompoundTypeTree(Template(lookups, noSelfType, Nil))
          else lookups.head
        }

        val stg =
          q"""
            object ${TermName(className.toString + "Type")} extends StructType[$className] with $parent {
              override def wrapped(x: Exp[$className]) = ${className.toTermName}(x)
              override def typeArguments = Nil
              override def stagedClass = classOf[$className]
              override def fields = Seq(..$childList)
            }
           """
        /**
          * Staged Evidence
          */
        val ev =
          q"""
            implicit def ${TermName(className.toString + "TypeEvidence")}: StructType[$className] = ${TermName(className.toString + "Type")}
          """

        /**
          * Constructor
          */
        val args = fields.map{
          case ValDef(_, termName, typeIdent, _) =>
            ValDef(Modifiers(Flag.PARAM), termName, typeIdent, EmptyTree)
        }
        val body = fields.map{
          case ValDef(_, termName, typeIdent, rhs) =>
            q"${Literal(Constant(termName.toString))} -> $termName.s"
        }

        // TODO: We assume for now that struct annotation is always used within a trait - any way to be more general?

        val mdef = q"def ${className.toTermName}(..$args): $className = struct[$className]( ..$body )"

        // Implicit object must come before class definition
        val cc = q"$stg ; $ev ; $cls ; $mdef ; ..$evidences"

        // Debugging
        //c.info(tree.pos, showCode(cc), force = true)
        //c.info(tree.pos, showRaw(cc), force = true)
        cc

      case _ =>
        c.error(c.enclosingPosition, "Only classes can be transformed using the @struct annotation")
        annottees.head
    }
  }
}
