package monads

/**
  * Design Patterns, Beispiele:
  *
  * 1.) Suche den Monoiden -> Contracts
  *
  * 2.) Suche nach dem Functor
  */

/**
  * eine Liste mit den Funktionen of und flatMap ist eine Monade (nicht: "eine Liste ist eine Monade")
  */
object Monads {

  trait Functor[F[_]] {
    def map[A, B](x: F[A])(f: A => B): F[B]
  }
  // Jede Monade ist ein Funktor, aber es gibt auch
  // Funktoren, die keine Monade sind

  // dazwischen: Applicative Functors
  //             Selective Functors

  //geht nur mit Typen mit 1 Typparameter
  trait Monad[M[_]] { // higher kinded type
    def of[A](result: A): M[A]
    def flatMap[A, B](m: M[A])(f: A => M[B]): M[B] // curryfied (m)(f)  In Haskell >>=, ausgesprochen "bind"

    //    def join[A](mm: M[M[A]]): M[A]
  }

  // einstellige Typkonstruktoren
  // Bsp:
  //   * Option[_]
  //   * List[_]

  def listFunctor: Functor[List] =
    new Functor[List] {
      override def map[A, B](x: List[A])(f: A => B): List[B] =
        x match {
          case head :: tail => f(head) :: map(tail)(f)
          case Nil          => Nil
        }
    }

  def listMonad: Monad[List] =
    new Monad[List] {
      override def of[A](result: A): List[A] = List(result)

      override def flatMap[A, B](m: List[A])(f: A => List[B]): List[B] =
        m match {
          case head :: tail => f(head) ++ flatMap(tail)(f)
          case Nil          => Nil
        }
    }

  def optionFunctor: Functor[Option] =
    new Functor[Option] {
      override def map[A, B](x: Option[A])(f: A => B): Option[B] =
        x match {
          case Some(value) => Option(f(value))
          case None        => None
        }
    }

  def optionMonad: Monad[Option] =
    new Monad[Option] {
      override def of[A](result: A): Option[A] = Some(result)

      override def flatMap[A, B](m: Option[A])(f: A => Option[B]): Option[B] =
        m match {
          case Some(value) => f(value)
          case None        => None
        }
    }

  // ============= Either[A, B] Zweistelliger Typkonstruktor

  // Left(a: A) extends Either[A, B]
  // Right(b: B) extends Either[A, B]
  // häufig: Either[Error, Result]

  def eitherFunctor[Error]: Functor[Either[Error, *]] =
    new Functor[Either[Error, *]] {
      override def map[A, B](x: Either[Error, A])(f: A => B): Either[Error, B] =
        x match {
          case Left(error)  => Left(error)
          case Right(value) => Right(f(value))
        }
    }

  // Either[Error, *]: ein-stelliger Typkonstruktor A => Either[Error, A]
  def eitherMonad[Error]: Monad[Either[Error, *]] =
    new Monad[Either[Error, *]] {
      override def of[A](result: A): Either[Error, A] = Right(result)

      override def flatMap[A, B](m: Either[Error, A])(f: A => Either[Error, B]): Either[Error, B] =
        m match {
          case Left(error)  => Left(error)
          case Right(value) => f(value)
        }
    }

  // ====================

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3)
    val y = listFunctor.map(x)(i => s"number${i.toString}")
    val z = listMonad.flatMap(x)(i => List(i - 1, i + 1))
    println(y)
    println(z)

    // ================ Either[A, B]

  }

  def safeDivide(a: Int, b: Int): Either[String, Int] = if (b == 0) Left("error") else Right(a / b)

  val x = for {
    d1 <- safeDivide(10, 3)
    d2 <- safeDivide(10, 0)
    d3 <- safeDivide(5, 2)
  } yield (d1 + d2 + d3)

  // ============= Reader Monade (für DB zugriffe z.B.) ===>> siehe jambit2021 repo Monads.scala

  case class Reader[Env, A](process: Env => A) {
    def map[B](f: A => B): Reader[Env, B] = readerFunctor[Env].map(this)(f)

    // def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] = readerMonad[Env].flatMap(this)(f)
  }

  def readerFunctor[Env]: Functor[Reader[Env, *]] =
    new Functor[Reader[Env, *]] {
      override def map[A, B](reader: Reader[Env, A])(f: A => B): Reader[Env, B] = Reader(env => f(reader.process(env)))
    }

//  def readerMonad[Env]: Monad[Reader[Env, *]] = new Monad[Reader[Env, *]] {
//    override def of[A](result: A): Reader[Env, A] = Reader(_ => result)
//
//    override def flatMap[A, B](reader: Reader[Env, A])
//                              (f: A => Reader[Env, B]): Reader[Env, B] = join(Reader(env => f(reader.process(env))))
//  }

  def get[Env]: Reader[Env, Env] = Reader(x => x)
}
