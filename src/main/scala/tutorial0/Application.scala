package tutorial0

object Application {

  def main(args: Array[String]): Unit = {
    val d1 = Dillo(Liveness.Alive, 10)
    println(Animal.feed(d1, 2))

    val p1 = Parrot("I need a cookie", 12)
    println(Animal.feed(p1, 5))

    val highway: List[Animal] = d1 :: Dillo(Liveness.Alive, 5) :: p1 :: Parrot("more cookies", 4) :: Nil
    println(Animal.runOverAnimals(highway))

    println(Animal.map(Animal.runOverAnimal, highway))
    println(Animal.map2(Animal.runOverAnimal, highway))
    println(highway.map(Animal.runOverAnimal))

    val highway2: List[Animal] = d1 :: Dillo(Liveness.Dead, 5) :: p1 :: Parrot("", 4) :: Nil
    println(Animal.filter(Animal.isDead, highway2))
    println(highway2.filter(Animal.isDead))

    val dillos = List(Dillo(Liveness.Alive, 10), Dillo(Liveness.Dead, 12))
    println(dillos.filter(i => i.liveness == Liveness.Alive))

  }
}
