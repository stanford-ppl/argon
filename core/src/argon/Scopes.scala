package argon

/** Class representing the result of a staged scope. */
sealed class Block[+R](
  val inputs:    Seq[Exp[_]],
  val result:    Exp[R],
  val effects:   Effects,
  val effectful: Seq[Sym[_]],
  val temp:      Freq
) {
  def tp: Type[_] = result.tp

  override def toString = {
    if (inputs.isEmpty) c"Block($result)"
    else
      c"""Block(${inputs.mkString("(", ",", ")")} => $result)"""
  }
  override def hashCode() = (result, effects, effectful, inputs, temp).hashCode()
  override def equals(x: Any) = x match {
    case that: Block[_] => that.result == this.result && that.effects == this.effects && that.effectful == this.effectful &&
      that.inputs == this.inputs && that.temp == this.temp
    case _ => false
  }
}
object Block {
  def apply[R](inputs: Seq[Exp[_]], result: Exp[R], effects: Effects, effectful: Seq[Sym[_]], temp: Freq): Block[R] = {
    new Block(inputs, result, effects, effectful, temp)
  }
  def unapply(arg: Any): Option[(Seq[Exp[_]], Exp[_], Effects, Seq[Sym[_]], Freq)] = arg match {
    case block: Block[_] => Some((block.inputs,block.result,block.effects,block.effectful,block.temp))
    case _ => None
  }
}

case class Lambda1[A,+R](
  input: Exp[A],
  override val result:    Exp[R],
  override val effects:   Effects,
  override val effectful: Seq[Sym[_]],
  override val temp:      Freq
) extends Block[R](Seq(input),result,effects,effectful,temp)

case class Lambda2[A,B,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  override val result:    Exp[R],
  override val effects:   Effects,
  override val effectful: Seq[Sym[_]],
  override val temp:      Freq
) extends Block[R](Seq(inputA,inputB),result,effects,effectful,temp)

case class Lambda3[A,B,C,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  inputC: Exp[C],
  override val result:    Exp[R],
  override val effects:   Effects,
  override val effectful: Seq[Sym[_]],
  override val temp:      Freq
) extends Block[R](Seq(inputA,inputB,inputC),result,effects,effectful,temp)

case class Lambda4[A,B,C,D,+R](
  inputA: Exp[A],
  inputB: Exp[B],
  inputC: Exp[C],
  inputD: Exp[D],
  override val result:    Exp[R],
  override val effects:   Effects,
  override val effectful: Seq[Sym[_]],
  override val temp:      Freq
) extends Block[R](Seq(inputA,inputB,inputC,inputD),result,effects,effectful,temp)
