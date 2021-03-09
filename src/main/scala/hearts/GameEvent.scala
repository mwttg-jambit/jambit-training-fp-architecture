package hearts

/**
 * Event Sourcing
 *
 * vs Data Warehouse (aktueller Zustand) wird verändert
 *    - Konsistenz durch Transactionen
 *    - in verteiltem Setting schwer performant & korrekt zu bekommen
 *
 * Event Sourcing
 * Logbuch über alles was passiert ist: Events
 *
 *  - alles, was passiert ist (in der Vergangenheit)
 *  - fachlich motiviert
 *  - darf redundant sein
 *  - Events können sehr lange leben
 *
 *
 * Gibt häufig auch Commands:
 *  - Wunsch, dass etwas in der Zukunft passieren soll
 *
 *  BITTE NICHT DIE SELBEN TYPEN / OBJEKTE für Event und Command
 *
 */

// siehe Jambit2021 repository for
sealed trait GameEvent

object GameEvent {
  case class PlayerTurnChanged(player: Player) extends GameEvent
  case class PlayerReceivedCards(player: Player, hand: Set[Card]) extends GameEvent
  case class GameStarted(players: List[Player]) extends GameEvent
  case class PlayerPlayedCard(player: Player, card: Card) extends GameEvent
  case class PlayerReceivedTrick(player: Player, trick: Trick) extends GameEvent
  case class TrickCompleted(trick: Trick) extends GameEvent
  case class GameFinished(winner: Player) extends GameEvent
  case class TrickSuitDetermined(suit: Suit) extends GameEvent
  case class StartPlayerDetermined(player: Player) extends GameEvent
}
