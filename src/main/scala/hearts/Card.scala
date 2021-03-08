package hearts

final case class Card(rank: Rank, suit: Suit) {
  def beats(other: Card): Option[Boolean] =
    if (this.suit == other.suit)
      Some(this.rank > other.rank)
    else
      None
}

object Card {
  def deck: Seq[Card] =
    Suit.all.flatMap { s =>
      Rank.all.map(r => Card(r, s))
    }
}
