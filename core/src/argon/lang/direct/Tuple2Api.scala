package argon.lang.direct

import argon.core._
import forge._

trait Tuple2Api {
  /** Static methods **/
  @api def pack[A:Type,B:Type](a: A, b: B): MTuple2[A,B] = MTuple2.pack(a,b)
  @api def pack[A:Type,B:Type](t: (A, B)): MTuple2[A,B] = MTuple2.pack(t)
  @api def unpack[A:Type,B:Type](t: MTuple2[A,B]): (A,B) = (t._1, t._2)
}
