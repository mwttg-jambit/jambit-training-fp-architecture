package pizza

sealed trait Ingredients

object Ingredients {

  final case object Salami extends Ingredients

  final case object Gammon extends Ingredients

  final case object Fungi extends Ingredients

  final case object Cheese extends Ingredients
}
