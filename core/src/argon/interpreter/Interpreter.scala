package argon.interpreter

import argon.core._
import argon.{ConstantGenFailedException, GenerationFailedException}
import argon.traversal.Traversal
import scala.math.BigDecimal

trait Interpreter extends Traversal {

  override val recurse: RecurseOpt = Never

  protected def needsFPType(tp: Type[_]): Boolean = false
  protected def spatialNeedsFPType(tp: Type[_]): Boolean = false  // TODO: Spatial specific - should not be here!

  protected def remap(tp: Type[_]): String = tp.toString
  protected def quoteConst(c: Const[_]): String =
    ???

  protected def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case s: Sym[_] => s"x${s.id}"
  }

  protected def quoteOrRemap(arg: Any): String = arg match {
    case p: Seq[_] => p.map(quoteOrRemap).mkString(", ")  // By default, comma separate Seq
    case s: Set[_] => s.map(quoteOrRemap).mkString(", ")  // TODO: Is this expected? Sets are unordered..
    case e: Exp[_] => quote(e)
    case m: Type[_] => remap(m)
    case s: String => s
    case c: Int => c.toString
    case b: Boolean => b.toString
    case l: Long => l.toString
    case l: BigDecimal => l.toString
    case _ => throw new RuntimeException(s"Could not quote or remap $arg (${arg.getClass})")
  }

  protected def interpretBlock(b: Block[_]): Unit = {
    visitBlock(b)
//    println("block", s"${b.result}")
  }

  var variables: Map[Sym[_], Any] = Map()

  def updateVar(lhs: Sym[_], x: Any): Unit =
    variables += ((lhs, x))
//  var registers: Map[Reg[_], Any] = Map()

  def eval[A](x: Any) = (x match {
    case Const(y) => y
    case s: Sym[_] => variables(s)
    case a@_ =>
      println("attempted to eval " +a)
      ???
  }).asInstanceOf[A]

  object EAny {
    def unapply(x: Any) = Some(eval[Any](x))
  }

  object EArray {
    def unapply(x: Any) = Some(eval[Array[Any]](x))
  }

  object EString {
    def unapply(x: Any) = Some(eval[String](x))
  }
  
  object EBigDecimal {
    def unapply(x: Any) = Some(eval[BigDecimal](x))
  }
  
  object EBoolean {
    def unapply(x: Any) = Some(eval[Boolean](x))
  }

  def matchNode = PartialFunction.empty[Op[_], Any]

  protected def interpretNode(lhs: Sym[_], rhs: Op[_]): Unit = {
    updateVar(lhs, matchNode(rhs))
  }  

  final override protected def preprocess[S:Type](block: Block[S]): Block[S] = {
    println()
    println("[Interpreter]")
    variables.foreach { case (key, e) => println(s"${key}: $e") }
    block
  }

  def debug() = {
    println("[variables content]")
    variables.foreach { case (key, e) => println(s"${key}: $e") }
  }
  final override protected def postprocess[S:Type](block: Block[S]): Block[S] = {
    println()    
    debug
    block
  }
  final override protected def visit(lhs: Sym[_], rhs: Op[_]) = interpretNode(lhs, rhs)
  final override protected def visitFat(lhs: Seq[Sym[_]], rhs: Def) = ???
}
