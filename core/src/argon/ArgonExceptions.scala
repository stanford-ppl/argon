package argon

import argon.core._

import scala.util.control.NoStackTrace

abstract class CompilerException(id: Int, msg: String)(implicit state: State) extends Exception(s"Internal exception #$id: $msg")

abstract class UserError(ctx: SrcCtx)(implicit state: State) {
  def console(): Unit
  console()
  error(ctx)
}
abstract class ProgramError()(implicit state: State) {
  state.logError()
}


class RecursiveScheduleException(result: Any, xs: List[String]) extends
  Exception(c"Recursive schedule while scheduling result $result"){
    error(c"Recursive schedule while scheduling result $result:")
    xs.foreach { x => error(s"  $x") }
  }

class TestBenchFailed(errs: Int)(implicit state: State) extends Exception(c"""Compilation failed with $errs ${plural(errs,"error","errors")}""") with NoStackTrace
class RunningFailed(exit: Int)(implicit state: State) extends Exception(c"Running compiled testbench failed with exit code $exit") with NoStackTrace

case class NullStateException() extends Exception {
  error("A node or block was staged without any state")
  error("This usually happens when Def.mirror is used in a Transformer.")
  error("Use Def.mirrorNode instead to avoid this issue.")
}

class GenerationFailedException(node: Def) extends Exception(c"Don't know how to generate node $node") with NoStackTrace
class ConstantGenFailedException(c: Const[_])(implicit state: State) extends Exception(c"Don't know how to generate constant $c (${c.c.getClass}) with type ${c.tp}") with NoStackTrace

class UninitializedEffectContextException()(implicit state: State) extends
  CompilerException(1, "Attempted to stage effectful node outside effect context")

case class RedefinedSymbolException(s: Sym[_], d1: Def, d2: Def)(implicit state: State) extends
  CompilerException(2, c"Symbol $s was redefined during staging"){
    error(c"$s was redefined during staging: ")
    error(c"First:  $s = $d1")
    error(c"Second: $s = $d2")
  }

/*case class IllegalStageHereException(save: List[Sym[_]], context: List[Sym[_]])(implicit state: State) extends
  CompilerException(3, "Staging effects 'here' did not leave outer information intact", {
    error("While staging effects 'here', saved context was not preserved:")
    error("Saved:")
    save.foreach { s => error(str(s)) }
    error("Context:")
    context.foreach { s => error(str(s)) }
  })*/

class CodegenException(codegen: String, lhs: Sym[_], rhs: Def)(implicit state: State) extends
  CompilerException(4, c"Don't know how to generate $codegen code for $lhs = $rhs"){
    error(c"[$codegen] Don't know how to generate code for $lhs = $rhs")
  }

class EffectsOrderException(implicit state: State) extends CompilerException(5, c"Violated ordering of effects")

class FlexEvaluationException(s: Sym[_])(implicit state: State) extends
  CompilerException(6, c"Unable to evaluate $s"){ error(c"Unable to flex evaluate ${str(s)}") }

class UndefinedMirrorException(x: Any)(implicit state: State) extends
  CompilerException(7, c"Undefined mirroring for $x")

class TraversalFailedToConvergeException(msg: String)(implicit state: State) extends
  CompilerException(9, msg){ error(msg) }

class TraversalFailedToCompleteException(msg: String)(implicit state: State) extends
  CompilerException(10, msg){ error(msg) }

class NoFieldException(s: Exp[_], index: String)(implicit state: State) extends
  CompilerException(11, c"Attempted to unwrap undefined field $index from record $s"){ def console() = {
    error(c"Attempted to unwrap undefined field $index from record ${str(s)}")
  }}

class IllegalSubstException(name: String, s: Exp[_], prev: Exp[_], attempted: Exp[_])(implicit state: State) extends
  CompilerException(12, c"Transformer $name encountered illegal substitution"){
    error(c"Transformer $name encountered illegal substitution: $s already had substitution $prev when attempting to add rule $s -> $attempted")
  }

class IllegalMirrorExpException(name: String, s: Exp[_])(implicit state: State) extends
  CompilerException(13, c"Transformer $name could not directly mirror single symbol $s - has fat definition"){
    error(c"Transformer $name could not directly mirror single symbol $s - has fat definition")
  }

class PretransformException(name: String, s: Exp[_], s2: Exp[_])(implicit state: State) extends
  CompilerException(14, c"Transformer $name encounted symbol $s with existing rule $s -> $s2."){
    error(c"Transformer $name encounted symbol $s with existing rule $s -> $s2.")
    error(c"If this is expected, allow pre-transformation using allowPretransform = true.")
  }

class UndefinedAccessPatternException(x: Exp[_])(implicit state: State) extends
  CompilerException(15, c"Symbol $x has no defined access pattern"){
    error(c"Symbol $x has no defined access pattern")
  }

class NoBitWidthException(tp: Type[_])(implicit state: State) extends
  CompilerException(16, c"Type $tp has no method for computing bit width"){
    error(c"Type $tp has no method for computing bit width")
  }

class UnsupportedBankingType(name: String, x: Exp[_])(implicit state: State) extends
  CompilerException(17, c"Memory $x has no rule for $name banking"){
    error(c"Memory $x has no rule for $name banking")
  }

class TupleSizeUnsupported(element: String, x: Exp[_])(implicit state: State) extends
  CompilerException(18, c"FieldApply $x has no rule for applying to $element th element"){
    error(c"FieldApply $x has no rule for $element element")
  }

class NDArrayException(array: Exp[_], name: String)(implicit state: State) extends
  CompilerException(21, c"Array $array ($name) has no good codegen for CPP"){
    error(c"Array $array ($name) has no good codegen for CPP")
  }

class AccumWithoutReduceFunctionException(reg: Exp[_], lhs: Exp[_])(implicit state: State) extends
  CompilerException(19, c"Register $reg claims to be an accumulator but has no reduction function assigned"){
    error(c"Register $reg has claims to be an accumulator but has no reduction function")
  }

class NoWireConstructorException(lhs: String)(implicit state: State) extends
  CompilerException(1025, c"""Cannot create new wire for $lhs"""){
    error(c"""Cannot create new wire for $lhs""")
  }
