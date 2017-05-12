package argon

sealed abstract class Freq

object Freq {
  case object Cold extends argon.Freq
  case object Hot  extends argon.Freq
  case object Normal extends argon.Freq

  def combine(a: Freq, b: Freq): Freq = (a,b) match {
    case (Cold, _) => Cold
    case (_, Cold) => Cold
    case (Hot, _)  => Hot
    case (_, Hot)  => Hot
    case _         => Normal
  }
}