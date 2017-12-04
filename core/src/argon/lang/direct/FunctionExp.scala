package argon.lang.direct

import argon.core._
import forge._

trait FunctionExp {
  // These are used by the macro expansion of @module to create staged function definitions and calls

  @internal def __decl[R:Type](l: List[Bound[_]], body: () => Exp[R]): Exp[Func[R]] = Func.decl(l, body)
  @internal def __call[R:Type](func: Exp[Func[R]], l: List[Exp[_]]): Exp[R] = Func.call(func, l)
}
