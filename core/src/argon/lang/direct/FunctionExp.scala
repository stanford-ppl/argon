package argon.lang.direct

import argon.core._
import forge._

trait FunctionExp {
  @internal def fun[A:Type,R:Type](func: Function1[A,R]): Func1[A,R] = Func.decl1(func)
  @internal def fun[A:Type,B:Type,R:Type](func: Function2[A,B,R]): Func2[A,B,R] = Func.decl2(func)
  @internal def fun[A:Type,B:Type,C:Type,R:Type](func: Function3[A,B,C,R]): Func3[A,B,C,R] = Func.decl3(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,R:Type](func: Function4[A,B,C,D,R]): Func4[A,B,C,D,R] = Func.decl4(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,R:Type](func: Function5[A,B,C,D,E,R]): Func5[A,B,C,D,E,R] = Func.decl5(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,R:Type](func: Function6[A,B,C,D,E,F,R]): Func6[A,B,C,D,E,F,R] = Func.decl6(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,R:Type](func: Function7[A,B,C,D,E,F,G,R]): Func7[A,B,C,D,E,F,G,R] = Func.decl7(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,R:Type](func: Function8[A,B,C,D,E,F,G,H,R]): Func8[A,B,C,D,E,F,G,H,R] = Func.decl8(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,R:Type](func: Function9[A,B,C,D,E,F,G,H,I,R]): Func9[A,B,C,D,E,F,G,H,I,R] = Func.decl9(func)
  @internal def fun[A:Type,B:Type,C:Type,D:Type,E:Type,F:Type,G:Type,H:Type,I:Type,J:Type,R:Type](func: Function10[A,B,C,D,E,F,G,H,I,J,R]): Func10[A,B,C,D,E,F,G,H,I,J,R] = Func.decl10(func)
}
