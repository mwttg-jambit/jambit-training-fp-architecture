package hearts

sealed trait GameCommand

object GameCommand {
  case class DealHands(hands: List[Set[Card]]) extends GameCommand
  case class PlayCard(player: Player, card: Card) extends GameCommand
}
