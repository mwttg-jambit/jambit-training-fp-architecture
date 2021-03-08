package tutorial0

import scala.annotation.tailrec

sealed trait Animal
final case class Dillo(liveness: Liveness, weight: Int) extends Animal

final case class Parrot(sentence: String, weight: Int) extends Animal

object Animal {

  def runOverAnimal(animal: Animal): Animal =
    animal match {
      case Dillo(_, weight)  => Dillo(Liveness.Dead, weight)
      case Parrot(_, weight) => Parrot("", weight)
    }

  def feed(animal: Animal, in: Int): Animal =
    animal match {
      case Dillo(liveness, weight) if liveness == Liveness.Alive => Dillo(liveness, weight + in)
      case Parrot(sentence, weight) if sentence != ""            => Parrot(sentence, weight + in)
    }

  def runOverAnimals(animals: List[Animal]): List[Animal] = {
    @tailrec
    def loop(current: List[Animal], accumulator: List[Animal]): List[Animal] =
      current match {
        case Nil          => accumulator.reverse
        case head :: tail => loop(tail, runOverAnimal(head) :: accumulator)
      }

    loop(animals, List.empty)
  }

  def map(f: Animal => Animal, animals: List[Animal]): List[Animal] = {
    @tailrec
    def loop(current: List[Animal], accumulator: List[Animal]): List[Animal] =
      current match {
        case Nil          => accumulator.reverse
        case head :: tail => loop(tail, f(head) :: accumulator)
      }

    loop(animals, List.empty)
  }

  // polymorphe high order Funktion
  def map2[A](f: A => A, list: List[A]): List[A] = {
    @tailrec
    def loop(current: List[A], accumulator: List[A]): List[A] =
      current match {
        case Nil          => accumulator.reverse
        case head :: tail => loop(tail, f(head) :: accumulator)
      }

    loop(list, List.empty)
  }

  def filter[A](f: A => Boolean, list: List[A]): List[A] = {
    @tailrec
    def loop(current: List[A], accumulator: List[A]): List[A] =
      current match {
        case Nil          => accumulator.reverse
        case head :: tail => loop(tail, if (f(head)) head :: accumulator else accumulator)
      }

    loop(list, List.empty)
  }

  def isDead(animal: Animal): Boolean =
    animal match {
      case Dillo(liveness, _)  => liveness == Liveness.Dead
      case Parrot(sentence, _) => sentence == ""
    }
}
