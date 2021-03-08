package hearts

sealed abstract class Rank(val value: Int) extends Ordered[Rank] {
  override def compare(that: Rank): Int = this.value - that.value
}

object Rank {

  case object Ace extends Rank(14)

  case object King extends Rank(13)

  case object Queen extends Rank(12)

  case object Jack extends Rank(11)

  case object Ten extends Rank(10)

  case object Nine extends Rank(9)

  case object Eight extends Rank(8)

  case object Seven extends Rank(7)

  case object Six extends Rank(6)

  case object Five extends Rank(5)

  case object Four extends Rank(4)

  case object Three extends Rank(3)

  case object Two extends Rank(2)

  val all: Seq[Rank] = Seq(
    Ace,
    King,
    Queen,
    Jack,
    Ten,
    Nine,
    Eight,
    Seven,
    Six,
    Five,
    Four,
    Three,
    Two
  )
}
