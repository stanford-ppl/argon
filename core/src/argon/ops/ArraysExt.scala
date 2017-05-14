package argon.ops

import argon._
import forge._

/**
  * Array operations are separated into two categories. ArrayOps is for use in all DSLs which allow at least the use
  * of command-line arguments. ArrayExtOps is for more complicated operations, including update and parallel patterns.
  */
trait ArrayExtApi extends ArrayExtExp with ArrayApi { self: ArgonApi =>



}

trait ArrayExtExp extends ArrayExp { self: ArgonExp => }
