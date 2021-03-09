package pizza

sealed trait Pizza

object Pizza {

  final case object PizzaBase extends Pizza

  final case class AddIngredients(ingredients: Ingredients, pizza: Pizza) extends Pizza
}
