package argon.core

import argon.graphs.GraphMetadata
import scala.collection.mutable

abstract class Metadata[T] { self =>
  type Tx = argon.transform.Transformer

  def meet(that: T): T = this.asInstanceOf[T]
  def join(that: T): T = this.asInstanceOf[T]
  def isEmpiric: Boolean = true
  def mirror(f: Tx): T

  def ignoreOnTransform: Boolean = false

  final def key = self.getClass
  override final def hashCode(): Int = key.hashCode()
}

class IRMetadata extends GraphMetadata[Metadata[_]] {
  def add[M<:Metadata[M]:Manifest](edge: Exp[_], m: M): Unit = this.add(edge, Some(m))
  def add[M<:Metadata[M]:Manifest](edge: Exp[_], m: Option[M]): Unit = {
    val k = keyOf[M]
    val meta = getMetadata(edge)
    val prev = meta.get(k).map(_.asInstanceOf[M])
    val entry = join(m, prev) //metaUpdate(m, prev)
    if (entry.isDefined) addMetadata(edge, entry.get)
    else if (prev.isDefined) removeMetadata(edge, prev.get)
  }

  def apply[M<:Metadata[M]:Manifest](edge: Exp[_]): Option[M] = {
    val k = keyOf[M]
    getMetadata(edge).get(k).map(_.asInstanceOf[M])
  }
  def get(edge: Exp[_]): Map[Class[_],Metadata[_]] = getMetadata(edge)
  def set(edge: Exp[_], m: Map[Class[_],Metadata[_]]): Unit = setMetadata(edge, m)
  def add(edge: Exp[_], m: Map[Class[_],Metadata[_]]): Unit = this.set(edge, this.get(edge) ++ m)

  def clearAll[M<:Metadata[M]:Manifest] = clearMetadata(keyOf[M])
}


abstract class Globaldata[T] { self =>
  type Tx = argon.transform.Transformer
  def mirror(f: Tx): T
  def staleOnTransform: Boolean = true

  final def key = self.getClass
  override final def hashCode(): Int = key.hashCode()
}

class GlobalMetadata {
  def keyOf[M<:Globaldata[M]:Manifest]: Class[M] = manifest[M].runtimeClass.asInstanceOf[Class[M]]

  private val data = mutable.HashMap[Class[_],Globaldata[_]]()

  private def add(k: Class[_], m: Globaldata[_]): Unit = { data += k -> m }
  def add[M<:Globaldata[M]:Manifest](m : M): Unit = { data += m.key -> m }
  def get[M<:Globaldata[M]:Manifest]: Option[M] = data.get(keyOf[M]).map(_.asInstanceOf[M])

  def invalidateStale(): Unit = data.foreach{case (k,v) => if (v.staleOnTransform) data.remove(k) }

  def copyTo(that: GlobalMetadata): Unit = data.foreach{case (k,v) => that.add(k,v) }
  def clear(): Unit = data.clear()
}