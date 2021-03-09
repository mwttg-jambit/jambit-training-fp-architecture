package prime

// Laziness
// Die ersten 100 Primzahlen berechnen

object PrimeLazy {

  // def natsFromNotLazy(n: Int): List[Int] = n :: natsFrom(n + 1)

  def natsFrom(n: Int): LazyList[Int] = n #:: natsFrom(n + 1)

  def strikeMultiples(n: Int, list: LazyList[Int]): LazyList[Int] = list.filter(number => number % n != 0)

  // Invariante: 1. Zahl von List ist eine Primzahl
  def sieve(list: LazyList[Int]): LazyList[Int] =
    list.head #:: sieve(strikeMultiples(list.head, list))

  def primes = sieve(natsFrom(2))

  def main(args: Array[String]): Unit = {
    println(natsFrom(2).take(10).toList)
    println(primes.take(100).toList)
  }
}
