package macros
import scala.reflect.macros.blackbox


object Bits extends TypeclassMacro {
  override def generateLookup(c: blackbox.Context)(name: c.TypeName): Option[c.Tree] = {
    import c.universe._
    Some( Ident(TypeName(name.toString + "CanBits")) )
  }

  override def generateImplementation(c: blackbox.Context)(tree: c.Tree) = {
    import c.universe._
    tree match {
      case ClassDef(_, className: TypeName, tparams, impl@Template(parents, selfType, bodyList)) =>
        val (fields, methods) = bodyList.partition { case _:ValDef => true case _ => false }
        val classTerm = className.toTermName
        val fieldNames = fields.map{case ValDef(_,name,_,_) => name }
        val fieldTypes = fields.map{case ValDef(_,_,typ,_)  => typ }

        val distinctChildren = fieldTypes.map(_.toString).distinct
        val typeMapping = fieldTypes.map{x => distinctChildren.indexWhere{y => x.toString == y} }

        val distinctTypes = distinctChildren.map{x => TypeName(x) }

        val bitEvidence = List.tabulate(distinctTypes.length){i => TermName("bits"+i) }
        val stgEvidence = List.tabulate(distinctTypes.length){i => TermName("tp"+i) }
        val evidences = bitEvidence ++ stgEvidence

        val bitEvidenceParams = distinctTypes.zip(bitEvidence).map{case (tp,term) =>
          q"$term: Bits[$tp]"
        }
        val stgEvidenceParams = distinctTypes.zip(stgEvidence).map{case (tp,term) =>
          q"$term: Type[$tp]"
        }
        val implicits = bitEvidenceParams ++ stgEvidenceParams

        val fieldBitEvidence = typeMapping.map{i => bitEvidence(i) }

        val zero = fieldBitEvidence.map{child => q"$child.zero" }
        val one  = fieldBitEvidence.map{child => q"$child.one" }

        val maxes = fieldNames.zipWithIndex.map{case (field,i) => q"val ${TermName("field"+i)} = max.map(_.$field)" }
        val rands = fieldBitEvidence.zipWithIndex.map{case (child,i) => q"$child.random(${TermName("field"+i)})"}
        val lens  = fieldBitEvidence.map{child => q"$child.length" }

        /**
          * Type class instance
          */
        val cls =
        q"""
          class ${TypeName(className.toString + "Bits")}()(implicit ..$implicits) extends Bits[$className] {
            override def zero(implicit ctx: SrcCtx): $className = $classTerm ( ..$zero ) // Calls constructor
            override def one(implicit ctx: SrcCtx): $className = $classTerm ( ..$one )   // Calls constructor
            override def random(max: Option[$className])(implicit ctx: SrcCtx): $className = {
              ..$maxes
              $classTerm (..$rands) // Calls constructor
            }
            override def length = List(..$lens).sum    // Other way to generate arbitrary summation?
          }
        """

        /**
          * Implicit type class evidence
          * (implicit chaining :( )
          */
        val imp =
          q"""
            implicit def ${TermName(classTerm.toString+"MayBits")}(implicit ..$implicits): Bits[$className] = new ${TypeName(className.toString + "Bits")}()(..$evidences)
          """

        /**
          * Type class "lookup"
          * hack to get type class evidence from Type[T]
          */
        val bitEvs = distinctTypes.zipWithIndex.map{case (tp,i) => q"bitFields($i).asInstanceOf[Bits[$tp]]" }
        val stgEvs = distinctTypes.zipWithIndex.map{case (tp,i) => q"distinctFields($i).asInstanceOf[Type[$tp]]" }
        val evs = bitEvs ++ stgEvs

        val lookup =
          q"""
            // Hack 2: The Hackening
            trait ${TypeName(className.toString + "CanBits")} extends CanBits[$className] { this: StructType[$className] =>
              override protected def getBits(children: Seq[Type[_]]): Option[Bits[$className]] = {
                val fieldTypes = this.fields.map(_._2)
                val distinctFields = List(..$typeMapping).map{i => fieldTypes(i) }
                val distinctBits = distinctFields.map{case Bits(bits) => Some(bits); case _ => None }
                if (distinctBits.forall(_.isDefined)) {
                  val bitFields = distinctBits.map(_.get)

                  // Seriously what even is this nonsense
                  val bits = ${TermName(classTerm.toString + "MayBits")}(..$evs)
                  Some(bits)
                }
                else None
              }
            }
          """

        List(cls, imp, lookup)
    }
  }
}
