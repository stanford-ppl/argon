package argon.core

import argon.utils.escapeConst
import scala.util.control.NoStackTrace


trait Exceptions extends Reporting {
  abstract class CompilerException(id: Int, msg: String, console: => Unit) extends
    Exception(s"Internal exception #$id: $msg") { console }

  class RecursiveScheduleException(result: Any, xs: List[String]) extends
    CompilerException(0, c"Recursive schedule while scheduling result $result", {
      error(c"Recursive schedule while scheduling result $result:")
      xs.foreach{x => error(s"  $x") }
    })

  class TestBenchFailed(errs: Int) extends Exception(c"""Compilation failed with $errs ${plural(errs,"error","errors")}""") with NoStackTrace
}

trait ArgonExceptions extends Exceptions { this: Statements =>

  final def str(lhs: Sym[_]): String = lhs match {
    case Def(rhs) => c"$lhs = $rhs"
    case Const(c) => c"$lhs = $c"
    case _ => c"$lhs [bound]"
  }
  final def str(lhs: List[Sym[_]]): String = lhs.head match {
    case Def(rhs) => c"$lhs = $rhs"
    case syms => c"$syms"
  }


  class UninitializedEffectContextException() extends
    CompilerException(1, "Attempted to stage effectful node outside effect context", ())

  case class RedefinedSymbolException(s: Sym[_], d1: Def, d2: Def) extends
    CompilerException(2, c"Symbol $s was redefined during staging", {
      error(c"$s was redefined during staging: ")
      error(c"First:  $s = $d1")
      error(c"Second: $s = $d2")
    })

  case class IllegalStageHereException(save: List[Sym[_]], context: List[Sym[_]]) extends
    CompilerException(3, "Staging effects 'here' did not leave outer information intact", {
      error("While staging effects 'here', saved context was not preserved:")
      error("Saved:")
      save.foreach { s => error(str(s)) }
      error("Context:")
      context.foreach { s => error(str(s)) }
    })

  class CodegenException(codegen: String, lhs: Sym[_], rhs: Def) extends
    CompilerException(4, c"Don't know how to generate $codegen code for $lhs = $rhs", {
      error(c"[$codegen] Don't know how to generate code for $lhs = $rhs")
    })

  class EffectsOrderException(res: Sym[_], expected: Seq[Stm], actual: Seq[Stm], missing: Seq[Stm]) extends
    CompilerException(5, c"Violated ordering of effects", {
      error(c"Violated ordering of effects while traversing block result: ")
      error(str(res))
      error("expected: ")
      expected.foreach{stm => error(c"  $stm")}
      error("actual: ")
      actual.foreach{stm => error(c"  $stm")}
      error("missing: ")
      missing.foreach{stm => error(c"  $stm")}
    })

  class FlexEvaluationException(s: Sym[_]) extends
    CompilerException(6, c"Unable to evaluate $s", {error(c"Unable to flex evaluate ${str(s)}") })

  class UndefinedMirrorException(x: Any) extends
    CompilerException(7, c"Undefined mirroring for $x", {})

  class TraversalFailedToConvergeException(msg: String) extends
    CompilerException(9, msg, {error(msg)})

  class TraversalFailedToCompleteException(msg: String) extends
    CompilerException(10, msg, {error(msg)})

  class NoFieldException(s: Sym[_], index: String) extends
    CompilerException(11, c"Attempted to unwrap undefined field $index from record $s", {
      error(c"Attempted to unwrap undefined field $index from record ${str(s)}")
    })

  // --- User errors
  abstract class UserError(ctx: SrcCtx, console: => Unit) {
    console
    error(ctx)
  }

  class IllegalMutableSharingError(s: Sym[_], aliases: Set[Sym[_]])(ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Illegal sharing of mutable objects: ")
    aliases.foreach{alias =>
      error(c"${mpos(alias)}:  ${str(alias)}")
    }
    //error(c"  Caused in this definition ${str(s)}")
  })

  class IllegalMutationError(s: Sym[_], mutated: Set[Sym[_]])(ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Illegal mutation of immutable symbols")
    mutated.foreach { mut =>
      error(c"${mpos(mut)}  ${str(mut)}")
    }
    //error(c"  Caused in this definition: ${str(s)}")
  })

  class UnsupportedCastError(x: Staged[_], y: Staged[_])(ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Casting from $x to $y is unsupported")
  })

  class UnsupportedLiftError(c: Any, x: Staged[_])(ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Unsupported lift: $c of type ${c.getClass} to $x")
  })

  class LiftOverflowError(tp: Staged[_], c: Any)(ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, u"Loss of precision detected in implicit lift: $tp cannot represent value ${escapeConst(c)}.")
    error(u"""Use the explicit annotation "${escapeConst(c)}.as[$tp]" to ignore this error.""")
  })

  class LiftUnderflowError(tp: Staged[_], c: Any)(ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, u"Loss of precision detected in implicit lift: $tp cannot represent value ${escapeConst(c)}.")
    error(u"""Use the explicit annotation "${escapeConst(c)}.as[${tp}]" to ignore this error.""")
  })

}
