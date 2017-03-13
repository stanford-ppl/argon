package argon.codegen.dotgen

import sys.process._
import scala.language.postfixOps
import argon.codegen.Codegen
import argon.Config
import argon.codegen.FileDependencies

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

trait DotCodegen extends Codegen with FileDependencies with DotEnum { // FileDependencies extends Codegen already
  import IR._
  override val name = "Dot Codegen"
  override val lang: String = "dot"
  override val ext: String = "dot"

  val edges = ListBuffer[()=>Any]() //Hack. change to bufferedStream

  override protected def emitBlock(b: Block[_]): Unit = {
    visitBlock(b)
  }

  override def quote(s: Exp[_]): String = s match {
    case c: Const[_] => quoteConst(c)
    case b: Bound[_] => s"b${b.id}"
    case Def(d) =>
      val defName = d.getClass.getSimpleName
      s"${super.quote(s)}:$defName"
    case lhs: Sym[_] => s"x${lhs.id}"
  }

  val regex = "\\[[0-9]*\\]".r
  def q(s:Any) = regex.replaceAllIn(s.toString, "")

  def emitblk[T](s:String)(block: =>T) = {
    open(s"$s {")
    val res = block
    close(s"}")
    res
  }
  def emitVert(n:Exp[_]):Unit = emitVert(n, attr(n))
  def emitVert(n:Any, label:Any):Unit = {
    emit(s"""${q(n)} [label="${q(label)}"];""")
  }
  def emitVert(n:Any, label:Any, attr:DotAttr):Unit = {
    emit(s"""${q(n)} [label="${q(label)}" ${attr.list} ];""")
  }
  def emitVert(n:Any, attr:DotAttr):Unit = {
    emit(s"""${q(n)} [${attr.list} ];""")
  }
  def emitEdge(from:Any, to:Any, label:String):Unit = {
    emitEdge(from, to, DotAttr().label(label))
  }
  def emitEdge(from:Any, to:Any, attr:DotAttr):Unit = {
    def buffered() = { emit(s"""${q(from)} -> ${q(to)} ${if (attr.attrMap.size!=0) s"[${attr.list}]" else ""}""") }
    edges += buffered _
  }
  def emitEdge(from:Any, to:Any):Unit = {
    def buffered() = { emit(s"""${q(from)} -> ${q(to)}""") }
    edges += buffered _
  }
  //def emitEdge(from:Any, ffield:Any, to:Any, tfield:Any):Unit = {
    //emitEdge(s"${from}:${ffield}", s"${to}:${tfield}")
  //}
  //def emitEdge(from:Any, ffield:Any, to:Any, tfield:Any, attr:DotAttr):Unit = {
    //emitEdge(s"${from}:${ffield}", s"${to}:${tfield}", attr)
  //}
  //def emitEdge(from:Any, ffield:Any, fd:String, to:Any, tfield:Any, td:String):Unit = {
    //emitEdge(s"${from}:${ffield}:${fd}", s"${to}:${tfield}:${td}")
  //}

  def emitSubGraph(n:Any, label:Any)(block: =>Any):Unit = {
		emitSubGraph(n, DotAttr().label(label.toString))(block)
	}
  def emitSubGraph(n:Any, attr:DotAttr)(block: =>Any):Unit = {
		emitblk(s"""subgraph cluster_${n}""") {
      emit(attr.expand)
			block
		}
  }

  def attr(n:Exp[_]):DotAttr = {
    DotAttr().label(quote(n))
  }

  override def copyDependencies(out: String): Unit = {
    val resourcesPath = s"${sys.env("SPATIAL_HOME")}/src/spatial/codegen/dotgen/resources"
    dependencies ::= AlwaysDep(s"""${resourcesPath}/run.sh""")
    super.copyDependencies(out)
    //val cmd = s"./$out/run.sh" 
    //Console.println(cmd)
    //s"$cmd" !
  }

}

class DotAttr() {
  val attrMap:Map[String, String] = Map.empty 
  val graphAttrMap:Map[String, String] = Map.empty 

  def + (rec:(String, String)):DotAttr = { attrMap += rec; this}

  def shape(s:Shape) = { attrMap += "shape" -> s.field; this }
  def color(s:Color) = { attrMap += "color" -> s.field; this }
  def fillcolor(s:Color) = { attrMap += "fillcolor" -> s.field; this }
  def labelfontcolor(s:Color) = { attrMap += "labelfontcolor" -> s.field; this }
  def style(ss:Style*) = { attrMap += "style" -> ss.map(_.field).mkString(","); this }
  def graphStyle(s:Style) = { graphAttrMap += "style" -> s"${s.field}"; this }
  def label(s:Any) = { attrMap += "label" -> s.toString; this }
  def label = { attrMap.get("label") }
  def dir(s:Direction) = { attrMap += "dir" -> s.field; this }
  def pos(coord:(Double,Double)) = { attrMap += "pos" -> s"${coord._1},${coord._2}!"; this }

  def elements:List[String] = {
    var elems = attrMap.map{case (k,v) => s"""$k="$v""""}.toList
    if (graphAttrMap.size!=0)
      elems = elems :+ s"graph[${graphAttrMap.map{case(k,v) => s"""$k="$v"""" }.mkString(",")}]"
    elems
  }
  def list = elements.mkString(",")
  def expand = elements.mkString(";") 
}
object DotAttr {
  def apply():DotAttr = new DotAttr()
  def copy(attr:DotAttr):DotAttr = {
    val newAttr = DotAttr()
    attr.attrMap.foreach { e => newAttr + e }
    newAttr
  }
}
trait DotField { val field:String }
case class Shape(field:String) extends DotField
case class Color(field:String) extends DotField
case class Style(field:String) extends DotField
case class Direction(field:String) extends DotField

trait DotEnum {
  val Mrecord   = Shape("Mrecord")
  val box       = Shape("box")
  val ellipse   = Shape("ellipse")
  val circle    = Shape("circle")
  val hexagon   = Shape("hexagon")

	val filled    = Style("filled")
  val bold      = Style("bold")
  val dashed    = Style("dashed")
  val rounded   = Style("rounded")
  val dotted    = Style("dotted")

	val white     = Color("white")
	val black     = Color("black")
	val lightgrey = Color("lightgrey")
  val gold      = Color("gold")
  val limegreen = Color("limegreen")
  val blue      = Color("blue")
  val red       = Color("red")
  val indianred = Color("indianred1")
  val cyan      = Color("cyan4")
  val pink      = Color("deeppink1")

  val both = Direction("both")

  implicit def field_to_string(f:DotField):String = f.field
}
