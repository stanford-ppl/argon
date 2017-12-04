package argon.core

/** Class representing the result of a staged scope. */
case class BlockProperties(
  temp:      Freq,
  isolated:  Boolean,
  seal:      Boolean,
  prog:      Boolean
) extends CompilerFacing {
  override def toStringCompiler: String = {
    s"{temp: $temp, isolated: $isolated, sealed: $seal, program: $prog}"
  }
}

sealed class Block[+R](
  val inputs:     Seq[Exp[_]],
  val result:     Exp[R],
  val effects:    Effects,
  val effectful:  Seq[Sym[_]],
  val properties: BlockProperties
) {
  def tp: Type[_] = result.tp

  def temp: Freq = properties.temp
  def isolated: Boolean = properties.isolated
  def seal: Boolean = properties.seal
  def prog: Boolean = properties.prog

  override def toString = {
    if (inputs.isEmpty) c"Block($result)"
    else
      c"""Block(${inputs.mkString("(", ",", ")")} => $result)"""
  }
  override def hashCode() = (result, effects, effectful, inputs, properties).hashCode()
  override def equals(x: Any) = x match {
    case that: Block[_] => that.result == this.result && that.effects == this.effects && that.effectful == this.effectful &&
      that.inputs == this.inputs && that.properties == this.properties
    case _ => false
  }
}

object Block {
  def apply[R](inputs: Seq[Exp[_]], result: Exp[R], effects: Effects, effectful: Seq[Sym[_]], properties: BlockProperties): Block[R] = {
    new Block(inputs, result, effects, effectful, properties)
  }
  def unapply(arg: Any): Option[(Seq[Exp[_]], Exp[_], Effects, Seq[Sym[_]], BlockProperties)] = arg match {
    case block: Block[_] => Some((block.inputs,block.result,block.effects,block.effectful,block.properties))
    case _ => None
  }

  def Program  = BlockProperties(Freq.Normal, isolated = false, seal = false, prog = true)
  def Sealed   = BlockProperties(Freq.Normal, isolated = false, seal = true,  prog = false)
  def Isolated = BlockProperties(Freq.Cold,   isolated = true,  seal = false, prog = false)
  def Normal   = BlockProperties(Freq.Normal, isolated = false, seal = false, prog = false)
  def Cold     = BlockProperties(Freq.Cold,   isolated = false, seal = false, prog = false)
  def Hot      = BlockProperties(Freq.Hot,    isolated = false, seal = false, prog = false)
}

case class Lambda1[A,+R](
  input: Exp[A],
  override val result:     Exp[R],
  override val effects:    Effects,
  override val effectful:  Seq[Sym[_]],
  override val properties: BlockProperties
) extends Block[R](Seq(input),result,effects,effectful,properties)

case class Lambda2[A,B,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  override val result:     Exp[R],
  override val effects:    Effects,
  override val effectful:  Seq[Sym[_]],
  override val properties: BlockProperties
) extends Block[R](Seq(inputA,inputB),result,effects,effectful,properties)

case class Lambda3[A,B,C,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  inputC: Exp[C],
  override val result:     Exp[R],
  override val effects:    Effects,
  override val effectful:  Seq[Sym[_]],
  override val properties: BlockProperties
) extends Block[R](Seq(inputA,inputB,inputC),result,effects,effectful,properties)

case class Lambda4[A,B,C,D,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  inputC: Exp[C],
  inputD: Exp[D],
  override val result:     Exp[R],
  override val effects:    Effects,
  override val effectful:  Seq[Sym[_]],
  override val properties: BlockProperties
) extends Block[R](Seq(inputA,inputB,inputC,inputD),result,effects,effectful,properties)

case class Lambda5[A,B,C,D,E,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  inputC: Exp[C],
  inputD: Exp[D],
  inputE: Exp[E],
  override val result:     Exp[R],
  override val effects:    Effects,
  override val effectful:  Seq[Sym[_]],
  override val properties: BlockProperties
) extends Block[R](Seq(inputA,inputB,inputC,inputD,inputE),result,effects,effectful,properties)

case class Lambda6[A,B,C,D,E,F,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  inputC: Exp[C],
  inputD: Exp[D],
  inputE: Exp[E],
  inputF: Exp[F],
  override val result:     Exp[R],
  override val effects:    Effects,
  override val effectful:  Seq[Sym[_]],
  override val properties: BlockProperties
) extends Block[R](Seq(inputA,inputB,inputC,inputD,inputE,inputF),result,effects,effectful,properties)
