package pizza

object Application {

  def main(args: Array[String]): Unit = {
    val pizza1 = Pizza.AddIngredients(Ingredients.Cheese, (Pizza.AddIngredients(Ingredients.Salami, Pizza.PizzaBase)))

    println(pizza1)
  }
}
