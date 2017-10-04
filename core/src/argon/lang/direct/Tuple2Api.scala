package argon.lang.direct

import argon.core._
import forge._

trait Tuple2Api {
  /** Static methods **/

  /** Returns a staged Tuple2 from the given unstaged Tuple2. **/
  @api def pack[A:Type,B:Type](t: (A, B)): MTuple2[A,B] = MTuple2.pack(t)

  /** Returns a staged Tuple2 from the given pair of values. Shorthand for ``pack((a,b))``. **/
  @api def pack[A:Type,B:Type](a: A, b: B): MTuple2[A,B] = MTuple2.pack(a,b)

  /** Returns an unstaged scala.Tuple2 from this staged Tuple2. Shorthand for ``(x._1, x._2)``. **/
  @api def unpack[A:Type,B:Type](t: MTuple2[A,B]): (A,B) = (t._1, t._2)
}
