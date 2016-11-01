package argon.core

abstract class CompilerException(msg: String, console: => Unit) extends
  Exception(s"Internal exception: $msg") { console }


trait ArgonExceptions extends Reporting { this: Statements =>

  final def str(lhs: Sym): String = lhs match {
    case Def(rhs) => c"$lhs = $rhs"
    case sym => c"$sym"
  }
  final def str(lhs: List[Sym]): String = lhs.head match {
    case Def(rhs) => c"$lhs = $rhs"
    case syms => c"$syms"
  }

  class UninitializedEffectContextException() extends
    CompilerException("Attempted to stage effectful node outside effect context", ())

  case class RedefinedSymbolException(s: Sym, d1: Def, d2: Def) extends
    CompilerException(c"Symbol $s was redefined during staging", {
      error(c"$s was redefined during staging: ")
      error(c"First:  $s = $d1")
      error(c"Second: $s = $d2")
    })

  case class IllegalStageHereException(save: List[Sym], context: List[Sym]) extends
    CompilerException("Staging effects 'here' did not leave outer information intact", {
      error("While staging effects 'here', saved context was not preserved:")
      error("Saved:")
      save.foreach { s => error(str(s)) }
      error("Context:")
      context.foreach { s => error(str(s)) }
    })

  class CodegenException(codegen: String, lhs: Sym, rhs: Def) extends
    CompilerException(c"Don't know how to generate $codegen code for $lhs = $rhs", {
      error(c"[$codegen] Don't know how to generate code for $lhs = $rhs")
    })

  class EffectsOrderException(res: Any, expected: Seq[Stm], actual: Seq[Stm], missing: Seq[Stm]) extends
    CompilerException(c"Violated ordering of effects", {
      error(c"Violated ordering of effects while traversing block result: ")
      res match {case s:Sym => error(str(s)); case _ => }
      error("expected: ")
      expected.foreach{stm => error(c"  $stm")}
      error("actual: ")
      actual.foreach{stm => error(c"  $stm")}
      error("missing: ")
      missing.foreach{stm => error(c"  $stm")}
    })

  class UndefinedNextException(tp: Typ[_]) extends
    CompilerException(c"Next is undefined for staged type $tp", {error(c"Next is undefined for staged type $tp") })

  class FlexEvaluationException(s: Sym) extends
    CompilerException(c"Unable to evaluate $s", {error(c"Unable to flex evaluate ${str(s)}") })

  class UndefinedMirrorException(x: Any) extends
    CompilerException(c"Undefined mirroring for $x", {})

  class TraversalFailedToConvergeException(msg: String) extends
    CompilerException(msg, {error(msg)})

  class TraversalFailedToCompleteException(msg: String) extends
    CompilerException(msg, {error(msg)})

  class NoFieldException(s: Sym, index: String) extends
    CompilerException(c"Attempted to unwrap undefined field $index from record $s", {
      error(c"Attempted to unwrap undefined field $index from record ${str(s)}")
    })

  // --- User errors
  abstract class UserException(msg: String, console: => Unit) extends Exception(s"Error: $msg") {
    console
  }

  case class IllegalMutableSharingError(s: Sym, aliases: Set[Sym])(ctx: SrcCtx) extends
    UserException(s"Illegal sharing of mutable objects $aliases ", {
      error(c"$ctx: Illegal sharing of mutable objects: ")
      aliases.foreach{alias =>
        error(c"${mpos(alias)}:  ${str(alias)}")
      }
      error(c"  Caused in this definition ${str(s)}")
    })

  case class IllegalMutationError(s: Sym, mutated: Set[Sym])(ctx: SrcCtx) extends
    UserException(c"Illegal mutation of immutable symbols $mutated", {
      error(c"$ctx: Illegal mutation of immutable symbols")
      mutated.foreach { mut =>
        error(c"${mpos(mut)}  ${str(mut)}")
      }
      error(c"  Caused in this definition: ${str(s)}")
    })

}
