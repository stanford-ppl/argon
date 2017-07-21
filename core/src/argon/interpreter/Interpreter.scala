package argon.interpreter

import argon.nodes._
import argon.core._
import argon.traversal.Traversal
import scala.math.BigDecimal
import java.util.concurrent.{ LinkedBlockingQueue => Queue }
import scala.collection.JavaConverters._

trait Interpreter extends Traversal {

  override val recurse: RecurseOpt = Never

  protected def interpretBlock(b: Block[_]): Exp[_]= {
    visitBlock(b).result
  }

  var variables: Map[Sym[_], Any] = Map()
  var bounds: Map[Bound[_], Any] = Map()

  def updateVar(lhs: Sym[_], x: Any): Unit =
    variables += ((lhs, x))

  def updateBound(bd: Bound[_], x: Any): Unit =
    bounds += ((bd, x))

  def removeBound(bd: Bound[_]) =
    bounds -= bd
  

  def eval[A](x: Exp[_]): A = (x match {
    //Internal const rewrites
//    case s@Const(y)  => y.asInstanceOf[Seq[(Any, Exp[_])]].map(x => (x._1, eval[Any](x._2)))
    //Otherwise
    case Const(y) => y
    case Param(y) => y
    case b: Bound[_] => bounds(b)
    case s: Sym[_] => variables(s)
    case a@_ =>
      println(s"[${Console.RED}error${Console.RESET}] Attempted to eval: $a ${a.getClass}")
      System.exit(0)
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

  object ESeq {
    def unapply(x: Exp[_]) = Some(eval[Seq[_]](x))
  }
  
  object SeqE {
    def unapply(x: Seq[Exp[_]]) = Some(x.map(eval[Any]))
  }

  object SeqEB {
    def unapply(x: Seq[Exp[_]]) = Some(x.map(eval[Boolean]))
  }

  object SeqEI {
    def unapply(x: Seq[Exp[_]]) = Some(x.map(x => eval[BigDecimal](x).toInt))
  }
  
  trait INodes
  
  def matchNode(lhs: Sym[_]) = PartialFunction.empty[Op[_], Any]

  def exit() = ()

  protected def interpretNode(lhs: Sym[_], rhs: Op[_]): Unit = {

    if (Config.verbosity >= 3) {
      println("Press a key to execute instruction (q to quit)")
      if (io.StdIn.readLine() == "q") {
        Config.exit()
        System.exit(0)
      }
    }

    val v = matchNode(lhs).lift(rhs).getOrElse({
      println()
      println(s"[${Console.RED}error${Console.RESET}] Unable to interpret this node: " + (lhs, rhs))
      displayInfo
      System.exit(0)
    })

//    if (!v.isInstanceOf[Unit])
    updateVar(lhs, v)

  }  

  final override protected def preprocess[S:Type](block: Block[S]): Block[S] = {
    println()
    println(s"[${Console.GREEN}Interpreter${Console.RESET}]")
    block
  }

  final override protected def postprocess[S:Type](block: Block[S]): Block[S] = {
    if (Config.verbosity >= 2) {    
      println()
      displayInfo
    }
    block
  }
  


  def displayPair(keye: (Any, Any)) = {
    val (key, e) = keye
    val v = Interpreter.stringify(e)
    println(s"${Console.MAGENTA}${key}${Console.RESET}: $v") 
  }

  def displayInfo() = {}

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
        if (s.size > 10) 
          "Seq(" + s.take(10).map(stringify).mkString(", ") + ", ...)"
        else
          "Seq(" + s.map(stringify).mkString(", ") + ")"          

      case s: Array[_] =>
        if (s.size > 10) 
          "Array(" + s.take(10).map(stringify).mkString(", ") + ", ...)"
        else
          "Array(" + s.map(stringify).mkString(", ") + ")"          
        
      case (a, b) => "(" + stringify(a) + ", "  + stringify(b) + ")"
      case s: String => '"' + s + '"'
      case Const(x) => "C(" + stringify(x) + ")"
      case null => "null"
      case x: BigDecimal => x.toString.take(5)
      case _ => x.toString
    }
  }  
}
