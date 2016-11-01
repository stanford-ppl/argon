package argon

import java.io.File

import org.apache.commons.lang3.StringEscapeUtils.escapeJava

import scala.collection.mutable

object utils {

  def escape(raw: String): String = "\"" + escapeJava(raw) + "\""
  def escapeChar(raw: Char): String = "'"+escapeJava(raw.toString)+"'"

  def deleteExts(path: String, ext: String) = {
    val files: Array[String] = Option(new File(path).list).map(_.filter(_.endsWith(ext))).getOrElse(Array.empty)
    files.foreach{filename =>
      val file = new File(path + java.io.File.separator + filename)
      file.delete()
    }
  }

  object single {
    private def singleVisit(x: Any)(func: Any => Unit): Unit = x match {
      case x: Iterator[_] => while (x.hasNext) func(x.next)
      case x: Iterable[_] => singleVisit(x.iterator)(func)
      case p: Product     => singleVisit(p.productIterator)(func)
      case _ => func(x)
    }

    def buildSet[A,B](blk: A => Iterable[B])(x: Iterable[A]): Set[B] = {
      val out = new mutable.SetBuilder[B, Set[B]](Set.empty[B])
      singleVisit(x){x => out ++= blk(x.asInstanceOf[A]) }
      out.result
    }
    def buildList[A,B](blk: A => Iterable[B])(x: Iterable[A]): List[B] = {
      val out = new mutable.ListBuffer[B]
      singleVisit(x){x => out ++= blk(x.asInstanceOf[A]) }
      out.result
    }

    def collectList[T](blk: PartialFunction[Any,T])(x:Any):List[T] = {
      val out = new mutable.ListBuffer[T]
      singleVisit(x){x => if (blk.isDefinedAt(x)) out += blk(x) }
      out.result
    }
    def collectSet[T](blk: PartialFunction[Any,T])(x: Any): Set[T] = {
      val out = new mutable.SetBuilder[T, Set[T]](Set.empty[T])
      singleVisit(x){x => if (blk.isDefinedAt(x)) out += blk(x) }
      out.result
    }

    def collectLists[T](blk: PartialFunction[Any,Iterable[T]])(x:Any):List[T] = {
      val out = new mutable.ListBuffer[T]
      singleVisit(x){x => if (blk.isDefinedAt(x)) out ++= blk(x) }
      out.result
    }
    def collectSets[T](blk: PartialFunction[Any,Iterable[T]])(x: Any): Set[T] = {
      val out = new mutable.SetBuilder[T, Set[T]](Set.empty[T])
      singleVisit(x){x => if (blk.isDefinedAt(x)) out ++= blk(x) }
      out.result
    }
  }

  object recursive {
    private def recurseVisit[T](x:Any)(func: PartialFunction[Any,Unit]):Unit = x match {
      case e if func.isDefinedAt(e) => func(e)
      case ss:Iterator[_] => while (ss.hasNext) { recurseVisit(ss.next)(func) }
      case ss:Iterable[_] => recurseVisit(ss.iterator)(func)
      case ss:Product => recurseVisit(ss.productIterator)(func)
      case _ =>
    }
    /*def buildSet[A, B](blk:A => Iterable[B])(x:Iterable[A]):Set[B] = {
      val out = new mutable.SetBuilder[B, Set[B]](Set.empty[B])
      recurseVisit(x)(blk andThen e => out ++= blk(x.asInstanceOf[A]) }
      out.result
    }
    def buildList[A, B](blk:A => Iterable[B])(x:Iterable[A]):List[B] = {
      val out = new mutable.ListBuffer[B]
      recurseVisit(x) { x => out ++= blk(x.asInstanceOf[A]) }
      out.result
    }*/
    def collectList[T](blk:PartialFunction[Any, T])(x:Any):List[T] = {
      val out = new mutable.ListBuffer[T]
      recurseVisit(x)(blk andThen(e => out += e))
      out.result
    }
    def collectSet[T](blk:PartialFunction[Any, T])(x:Any):Set[T] = {
      val out = new mutable.SetBuilder[T, Set[T]](Set.empty[T])
      recurseVisit(x)(blk andThen(e => out += e))
      out.result
    }
    def collectLists[T](blk:PartialFunction[Any, Iterable[T]])(x:Any):List[T] = {
      val out = new mutable.ListBuffer[T]
      recurseVisit(x)(blk andThen(e => out ++= e))
      out.result
    }
    def collectSets[T](blk:PartialFunction[Any, Iterable[T]])(x:Any):Set[T] = {
      val out = new mutable.SetBuilder[T, Set[T]](Set.empty[T])
      recurseVisit(x)(blk andThen(e => out ++= e))
      out.result
    }
  }

}
