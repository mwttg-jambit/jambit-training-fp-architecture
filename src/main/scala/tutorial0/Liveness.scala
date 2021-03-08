package tutorial0

sealed trait Liveness

object Liveness {
  case object Dead extends Liveness
  case object Alive extends Liveness
}
