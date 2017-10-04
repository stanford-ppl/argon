package argon.core

package object cake {
  type Type[T] = argon.core.Type[T]
  type Lift[A,B] = argon.core.Lift[A,B]
  type Cast[A,B] = argon.core.Cast[A,B]

  type FrontendFacing = argon.core.FrontendFacing
  type CompilerFacing = argon.core.CompilerFacing

  type Freq = argon.core.Freq
  val Freq = argon.core.Freq

  type Exp[+T] = argon.core.Exp[T]
  type Dyn[+T] = argon.core.Dyn[T]
  type Sym[+T] = argon.core.Sym[T]
  type Bound[+T] = argon.core.Bound[T]
  type Const[+T] = argon.core.Const[T]
  val Const = argon.core.Const
  type Param[+T] = argon.core.Param[T]
  val Param = argon.core.Param

  type Def = argon.core.Def
  val Def = argon.core.Def
  type Op[T] = argon.core.Op[T]
  val Op = argon.core.Op
  type Op2[A,T] = argon.core.Op2[A,T]
  type Op3[A,B,T] = argon.core.Op3[A,B,T]
  type Op4[A,B,C,T] = argon.core.Op4[A,B,C,T]
  type Op5[A,B,C,D,T] = argon.core.Op5[A,B,C,D,T]
  type AtomicRead[C] = argon.core.AtomicRead[C]

  type Block[+T] = argon.core.Block[T]
  val Block = argon.core.Block
  type Lambda1[A,+T] = argon.core.Lambda1[A,T]
  val Lambda1 = argon.core.Lambda1
  type Lambda2[A,B,+T] = argon.core.Lambda2[A,B,T]
  val Lambda2 = argon.core.Lambda2
  type Lambda3[A,B,C,+T] = argon.core.Lambda3[A,B,C,T]
  val Lambda3 = argon.core.Lambda3
  type Lambda4[A,B,C,D,+T] = argon.core.Lambda4[A,B,C,D,T]
  val Lambda4 = argon.core.Lambda4
  type Lambda5[A,B,C,D,E,+T] = argon.core.Lambda5[A,B,C,D,E,T]
  val Lambda5 = argon.core.Lambda5
  type Lambda6[A,B,C,D,E,F,+T] = argon.core.Lambda6[A,B,C,D,E,F,T]
  val Lambda6 = argon.core.Lambda6

  type Lattice[T] = argon.core.Lattice[T]
  type Metadata[M] = argon.core.Metadata[M]
  type Effects = argon.core.Effects
  val Effects = argon.core.Effects
  type AntiDeps = argon.core.AntiDeps
  val AntiDeps = argon.core.AntiDeps

  type Stm = argon.core.Stm
  val Stm = argon.core.Stm
  val TP = argon.core.TP
  val TTP = argon.core.TTP

  type State = argon.core.State
}
