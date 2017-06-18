package argon.lang

import argon.core._
import argon.nodes._
import forge._

case class HashIndex[K:Type](s: Exp[HashIndex[K]]) extends MetaAny[HashIndex[K]] {
  //val mK: MetaAny[K] = typ[K].fake
  override type Internal = scala.collection.immutable.HashMap[Any,scala.Int]

  @api def =!=(x: HashIndex[K]): MBoolean = ??? // TODO! but never seen by user currently
  @api def ===(x: HashIndex[K]): MBoolean = ??? // TODO! but never seen by user currently
  @api def toText: MString = String.ify(this)
}

object HashIndex {
  implicit def hashIndexIsStaged[K:Type]: Type[HashIndex[K]] = HashIndexType(typ[K])
}
