package argon.core

import ops._
import scala.util.control.NoStackTrace

// TODO: Move these elsewhere?

abstract class CompilerException(id: Int, msg: String, console: => Unit) extends
  Exception(s"Internal exception #$id: $msg") { console }

abstract class UserError(ctx: SrcCtx, err: => Unit)(implicit state: State) {
  err
  error(ctx)
}
abstract class ProgramError(err: => Unit)(implicit state: State) {
  err
  state.logError()
}


class RecursiveScheduleException(result: Any, xs: List[String]) extends
  CompilerException(0, c"Recursive schedule while scheduling result $result", {
    error(c"Recursive schedule while scheduling result $result:")
    xs.foreach{x => error(s"  $x") }
  })

class TestBenchFailed(errs: Int) extends Exception(c"""Compilation failed with $errs ${plural(errs,"error","errors")}""") with NoStackTrace
class RunningFailed(exit: Int) extends Exception(c"Running compiled testbench failed with exit code $exit") with NoStackTrace

class GenerationFailedException(node: Def) extends Exception(c"Don't know how to generate node $node") with NoStackTrace
class ConstantGenFailedException(c: Const[_])(implicit state: State) extends Exception(c"Don't know how to generate constant $c (${c.c.getClass}) with type ${c.tp}") with NoStackTrace

class UninitializedEffectContextException() extends
  CompilerException(1, "Attempted to stage effectful node outside effect context", ())

case class RedefinedSymbolException(s: Sym[_], d1: Def, d2: Def) extends
  CompilerException(2, c"Symbol $s was redefined during staging", {
    error(c"$s was redefined during staging: ")
    error(c"First:  $s = $d1")
    error(c"Second: $s = $d2")
  })

/*case class IllegalStageHereException(save: List[Sym[_]], context: List[Sym[_]])(implicit state: State) extends
  CompilerException(3, "Staging effects 'here' did not leave outer information intact", {
    error("While staging effects 'here', saved context was not preserved:")
    error("Saved:")
    save.foreach { s => error(str(s)) }
    error("Context:")
    context.foreach { s => error(str(s)) }
  })*/

class CodegenException(codegen: String, lhs: Sym[_], rhs: Def) extends
  CompilerException(4, c"Don't know how to generate $codegen code for $lhs = $rhs", {
    error(c"[$codegen] Don't know how to generate code for $lhs = $rhs")
  })

class EffectsOrderException extends CompilerException(5, c"Violated ordering of effects", {})

class FlexEvaluationException(s: Sym[_])(implicit state: State) extends
  CompilerException(6, c"Unable to evaluate $s", {error(c"Unable to flex evaluate ${str(s)}") })

class UndefinedMirrorException(x: Any) extends
  CompilerException(7, c"Undefined mirroring for $x", {})

class TraversalFailedToConvergeException(msg: String) extends
  CompilerException(9, msg, {error(msg)})

class TraversalFailedToCompleteException(msg: String) extends
  CompilerException(10, msg, {error(msg)})

class NoFieldException(s: Exp[_], index: String)(implicit state: State) extends
  CompilerException(11, c"Attempted to unwrap undefined field $index from record $s", {
    error(c"Attempted to unwrap undefined field $index from record ${str(s)}")
  })

class IllegalSubstException(name: String, s: Exp[_], prev: Exp[_], attempted: Exp[_]) extends
  CompilerException(12, c"Transformer $name encountered illegal substitution: $s already had substitution $prev when attempting to add rule $s -> $attempted", {
    error(c"Transformer $name encountered illegal substitution: $s already had substitution $prev when attempting to add rule $s -> $attempted")
  })

class IllegalMirrorExpException(name: String, s: Exp[_]) extends
  CompilerException(13, c"Transformer $name could not directly mirror single symbol $s - has fat definition", {
    error(c"Transformer $name could not directly mirror single symbol $s - has fat definition")
  })

class PretransformException(name: String, s: Exp[_], s2: Exp[_]) extends
  CompilerException(14, c"Transformer $name encounted symbol $s with existing rule $s -> $s2.", {
    error(c"Transformer $name encounted symbol $s with existing rule $s -> $s2.")
    error(c"If this is expected, allow pre-transformation using allowPretransform = true.")
  })

class UndefinedAccessPatternException(x: Exp[_]) extends
  CompilerException(15, c"Symbol $x has no defined access pattern", {
    error(c"Symbol $x has no defined access pattern")
  })

class NoBitWidthException(tp: Type[_]) extends
  CompilerException(16, c"Type $tp has no method for computing bit width", {
    error(c"Type $tp has no method for computing bit width")
  })

class UnsupportedBankingType(name: String, x: Exp[_]) extends
  CompilerException(17, c"Memory $x has no rule for $name banking", {
    error(c"Memory $x has no rule for $name banking")
  })

class TupleSizeUnsupported(element: String, x: Exp[_]) extends
  CompilerException(18, c"FieldApply $x has no rule for applying to $element th element", {
    error(c"FieldApply $x has no rule for $element element")
  })

class AccumWithoutReduceFunctionException(reg: Exp[_], lhs: Exp[_]) extends
  CompilerException(19, c"Register $reg claims to be an accumulator but has no reduction function assigned", {
    error(c"Register $reg has claims to be an accumulator but has no reduction function")
  })

class UnusedDRAMException(dram: Exp[_], name: String) extends
  CompilerException(20, c"DRAM $dram ($name) was declared as a DRAM in app but is not used by the accel", {
    error(c"DRAM $dram ($name) was declared as a DRAM in app but is not used by the accel")
  })

class NDArrayException(array: Exp[_], name: String) extends
  CompilerException(21, c"Array ${array} ($name) has no good codegen for CPP", {
    error(c"Array ${array} ($name) has no good codegen for CPP")
  })

class OuterLevelInnerStyleException(name: String) extends
  CompilerException(22, c"Controller ${name} claims to be an outer level controller but has style of an innerpipe", {
    error(c"Controller ${name} claims to be an outer level controller but has style of an innerpipe")
  })

class DoublyUsedDRAMException(dram: Exp[_], name: String) extends
  CompilerException(23, c"DRAM $dram is used twice as a $name.  Please only load from a DRAM once, or else stream signals will interfere", {
    error(c"DRAM $dram is used twice as a $name.  Please only load from a DRAM once, or else stream signals will interfere")
  })

class TrigInAccelException(lhs: Exp[_]) extends
  CompilerException(24, c"""Cannot handle trig functions inside of accel block! ${lhs.name.getOrElse("")}""", {
    error(c"""Cannot handle trig functions inside of accel block! ${lhs.name.getOrElse("")}""")
  })

class NoWireConstructorException(lhs: String) extends
  CompilerException(25, c"""Cannot create new wire for $lhs""", {
    error(c"""Cannot create new wire for $lhs""")
  })
