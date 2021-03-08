package financial

sealed trait Currency
object Currency {
  case object EUR extends Currency
  case object GBP extends Currency
}
