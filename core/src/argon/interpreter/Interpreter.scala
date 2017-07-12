package argon.interpreter

import argon.nodes._
import argon.core._
import argon.traversal.Traversal
import scala.math.BigDecimal
import java.util.concurrent.{ LinkedBlockingQueue => Queue }
import scala.collection.JavaConverters._

trait Interpreter extends Traversal {

  override val recurse: RecurseOpt = Never

  protected def interpretBlock(b: Block[_]): Unit = {
    visitBlock(b)
//    println("block", s"${b.result}")
  }

  var variables: Map[Sym[_], Any] = Map()

  def updateVar(lhs: Sym[_], x: Any): Unit =
    variables += ((lhs, x))


  def eval[A](x: Exp[_]): A = (x match {
    //Internal const rewrites
    case s@Const(y) if s.tp.isInstanceOf[StructType[_]] => y.asInstanceOf[Seq[(Any, Exp[_])]].map(x => (x._1, eval[Any](x._2)))
    //Otherwise
    case Const(y) => y
    case Param(y) => y
    case s: Sym[_] => variables(s)
    case a@_ =>
      println("attempted to eval " +a)
      ???
  }).asInstanceOf[A]

  object EAny {
    def unapply(x: Exp[_]) = Some(eval[Any](x))
  }

  object EArray {
    def unapply(x: Exp[_]) = Some(eval[Array[Any]](x))
  }

  object EString {
    def unapply(x: Exp[_]) = Some(eval[String](x))
  }
  
  object EBigDecimal {
    def unapply(x: Exp[_]) = Some(eval[BigDecimal](x))
  }

  object EInt {
    def unapply(x: Exp[_]) = Some(eval[BigDecimal](x).toInt)
  }
  
  object EBoolean {
    def unapply(x: Exp[_]) = Some(eval[Boolean](x))
  }

  trait INodes
  
  def matchNode(lhs: Sym[_]) = PartialFunction.empty[Op[_], Any]

  def exit() = ()

  protected def interpretNode(lhs: Sym[_], rhs: Op[_]): Unit = {

    if (Config.debug) {
      println("Press a key to execute instruction (q to quit)")
      if (io.StdIn.readLine() == "q") {
        Config.exit()
        System.exit(0)
      }
    }

    updateVar(lhs, matchNode(lhs).lift(rhs).getOrElse({
      println("Unable to interpret this node " + (lhs, rhs))
      System.exit(0)
      ???
    }))

  }  

  final override protected def preprocess[S:Type](block: Block[S]): Block[S] = {
    println()
    println(s"[${Console.GREEN}Interpreter${Console.RESET}]")
    variables.foreach { case (key, e) => println(s"${key}: $e") }
    block
  }



  def debug() = {
    println(s"[${Console.BLUE}variables content${Console.RESET}]")
    variables.foreach { case (key, e) => { val v = Interpreter.stringify(e); println(s"${Console.MAGENTA}${key}${Console.RESET}: $v") }}
  }
  final override protected def postprocess[S:Type](block: Block[S]): Block[S] = {
    println()    
    debug
    block
  }
  final override protected def visit(lhs: Sym[_], rhs: Op[_]) = interpretNode(lhs, rhs)
  final override protected def visitFat(lhs: Seq[Sym[_]], rhs: Def) = ???
}

object Interpreter {
  def stringify(x: Any): String = {
    x match {
      case st: Stream[_] => st.toString
      case q: Queue[_] =>
        "Queue(" + q.asScala.toList.map(stringify).mkString(", ") + ")"
      case s: Seq[_] =>
        "Seq(" + s.map(stringify).mkString(", ") + ")"
      case (a, b) => "(" + stringify(a) + ", "  + stringify(b) + ")"
      case s: String => '"' + s + '"'
      case Const(x) => stringify(x)
      case null => "null"
      case _ => x.toString
    }
  }  
}
