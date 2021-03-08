package tutorial0


object Tutorial0 {

  def main(args: Array[String]): Unit = {
    val d1 = Dillo(Liveness.Alive, 10)
    println(Animal.feed(d1, 2))

    val p1 = Parrot("I need a cookie", 12)
    println(Animal.feed(p1, 5))
  }
}
