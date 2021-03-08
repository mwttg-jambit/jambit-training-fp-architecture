package tutorial0

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
      case Parrot(sentence, weight) if sentence != ""            =>  Parrot(sentence, weight + in)
    }
}
