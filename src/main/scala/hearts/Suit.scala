package hearts

trait Suit

object Suit {
  case object Heart extends Suit

  case object Spade extends Suit

  case object Diamond extends Suit

  case object Club extends Suit

  val all: Seq[Suit] = Seq(Heart, Spade, Diamond, Club)
}
