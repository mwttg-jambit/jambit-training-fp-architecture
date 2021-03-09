package math

object Math {

  /**
    * Gruppe, Mathematik
    *
    * - Menge M
    * - Binäre Operation, assoziativ: op(a, op(b, c) ~~op(op(a, b), c)
    * - neutrales Element n bezüglich op(n, x) ~~ n ~~op(x, n)
    * - zu jedem Element n ein inverses Element n^^1: op(x, x^-1) = n
    *
    * Menge n mit assoiativer Operation = Halbgruppe
    * Halbgroupe (semigroup) mit neutralen Element == Monoid
    *
    *
    * Programmierung:
    * statt Menge M gibt es einen Typen M
    */

  /*
  // First try
  trait Semigroup[M] {
    def op(other: M): M
  }

  trait Monoid[M] extends Semigroup[M] {
    def n: M // <-- will not work
  }

   */

  trait Semigroup[M] {
    def op(a: M, b: M): M
  }

  val additiveSemigroup: Semigroup[Int] = new Semigroup[Int] {
    def op(a: Int, b: Int): Int = a + b
  }

  class AdditiveSemigroup() extends Semigroup[Int] { // Type-Class
    override def op(a: Int, b: Int): Int = a + b
  }

  class MultiplySemigroup() extends Semigroup[Int] { // Type-Class
    override def op(a: Int, b: Int): Int = a * b
  }

  // -----------------------------------------

  // without implicit
  val foo                                           = additiveSemigroup.op(1, 2)
  def op[M](a: M, b: M)(semigroup: Semigroup[M]): M = semigroup.op(a, b)
  val bar                                           = op(1, 2)(additiveSemigroup)

  // implicit
  implicit val additiveSemigroup2                             = new AdditiveSemigroup()
  def op2[M](a: M, b: M)(implicit semigroup: Semigroup[M]): M = semigroup.op(a, b)
  val bar2                                                    = op2(4, 7)

  // -----------------------------------------

  // Monoid

  trait Monoid[M] extends Semigroup[M] {
    def neutral: M
  }

  class AdditiveMonoid() extends Monoid[Int] {
    override def neutral: Int = 0

    override def op(a: Int, b: Int): Int = a + b
  }

  class AdditiveMonoid2() extends AdditiveSemigroup with Monoid[Int] {
    override def neutral: Int = 0
  }

  val additiveMonoid = new AdditiveMonoid()

  implicit val additiveMonoid2 = new AdditiveMonoid()

  /*
  def monoidFold[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list match {
      case Nil          => monoid.neutral
      case head :: tail => monoid.op(head, monoidFold(tail)(monoid))
    }
  */

  // (nahezu gleich bedeutend wie oben
  def monoidFold[A: Monoid](list: List[A]): A =
    list match {
      case Nil          => implicitly[Monoid[A]].neutral
      case head :: tail => implicitly[Monoid[A]].op(head, monoidFold(tail))
    }

  //------------------------------------

  case class MonoidPackage[M](value: M, monoid: Monoid[M]) {
    def op(other: M): M = monoid.op(value, other)
  }

  implicit def pack[M](value: M)(implicit monoid: Monoid[M]): MonoidPackage[M] = MonoidPackage(value, monoid)

  val x = 5.op(7) // == pack(5)(additiveMonoid2).op(7)
  val y = pack(5)(additiveMonoid2).op(7)
  val z = pack(5).op(7)

  def main(args: Array[String]): Unit = {
    println(x)
    println(y)
    println(z)
  }
}
