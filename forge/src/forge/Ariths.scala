package forge
import scala.reflect.macros.blackbox

object Ariths extends TypeclassMacro {

  override def generateLookup(c: blackbox.Context)(name: c.TypeName): Option[c.Tree] = {
    None
  }

  override def generateImplementation(c: blackbox.Context)(tree: c.Tree) = {
    import c.universe._
    tree match {
      case ClassDef(_, className: TypeName, tparams, impl@Template(parents, selfType, bodyList)) =>
        val (fields, methods) = bodyList.partition { case _: ValDef => true case _ => false }
        val classTerm = className.toTermName
        val fieldNames = fields.map { case ValDef(_, name, _, _) => name }
        val fieldTypes = fields.map { case ValDef(_, _, typ, _) => typ }
        val distinctChildren = fieldTypes.map(_.toString).distinct
        val typeMapping = fieldTypes.map { x => distinctChildren.indexWhere { y => x.toString == y } }
        val distinctTypes = distinctChildren.map { x => TypeName(x) }
        val arithEvidence = List.tabulate(distinctTypes.length) { i => TermName("arith" + i) }
        val stgEvidence = List.tabulate(distinctTypes.length) { i => TermName("tp" + i) }
        val evidences = arithEvidence ++ stgEvidence
        val arithEvidenceParams = distinctTypes.zip(arithEvidence).map { case (tp, term) =>
          q"$term: Arith[$tp]"
        }
        val stgEvidenceParams = distinctTypes.zip(stgEvidence).map { case (tp, term) =>
          q"$term: Type[$tp]"
        }
        val implicits = arithEvidenceParams ++ stgEvidenceParams
        val fieldAriths = typeMapping.map { i => arithEvidence(i) }

        val aFields = List.tabulate(fields.length){i => TermName("fieldA" + i) }
        val bFields = List.tabulate(fields.length){i => TermName("fieldB" + i) }

        val aFieldsGet = fieldNames.zip(aFields).map{case (field,term) => q"val $term = a.$field" }
        val bFieldsGet = fieldNames.zip(bFields).map{case (field,term) => q"val $term = b.$field" }

        val neg = aFields.zip(fieldAriths).map{case (term,arith) => q"$arith.negate($term)" }
        val plus = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.plus($a,$b)" }
        val minus = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.minus($a,$b)" }
        val times = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.times($a,$b)" }
        val divide = (aFields, bFields, fieldAriths).zipped.map{case (a, b, arith) => q"$arith.divide($a,$b)" }

        /**
          * Type class instance
          */
        val cls =
          q"""
            class ${TypeName(className.toString + "Arith")}()(implicit ..$implicits) extends Arith[$className] {
              override def negate(a: $className)(implicit ctx: SrcCtx): $className = {
                ..$aFieldsGet
                $classTerm ( ..$neg )
              }
              override def plus(a: $className, b: $className)(implicit ctx: SrcCtx): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$plus )
              }
              override def minus(a: $className, b: $className)(implicit ctx: SrcCtx): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$minus )
              }
              override def times(a: $className, b: $className)(implicit ctx: SrcCtx): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$times )
              }
              override def divide(a: $className, b: $className)(implicit ctx: SrcCtx): $className = {
                ..$aFieldsGet
                ..$bFieldsGet
                $classTerm ( ..$divide )
              }

            }
          """
        /**
          * Implicit type class evidence
          * (implicit chaining :( )
          */
        val imp =
          q"""
              implicit def ${TermName(classTerm.toString + "MayArith")}(implicit ..$implicits): Arith[$className] = new ${TypeName(className.toString + "Arith")}()(..$evidences)
            """
        /**
          * Type class "lookup"
          * hack to get type class evidence from Type[T]
          */
        // Not needed for now

       List(cls, imp)
    }
  }
}
