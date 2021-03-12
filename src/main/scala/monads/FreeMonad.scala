package monads

import cats._
import cats.implicits._
import cats.data._


// trennen des "monadisch seins" vom Typen (z.B. DB, Netzwerk, etcc)
object FreeMonad {

  // Reader, aber diesmal als Datentyp, unabhängig von der Implementierung wie DB
  sealed trait Reader1[Env, A]
  case class Ask1[Env, A](callback: Env => Reader1[Env, A]) extends Reader1[Env, A]
  case class Return1[Env, A](result: A) extends Reader1[Env, A]

  def ask1[Env]: Reader1[Env, Env] = Ask1(Return1(_))

  // F ist nachher zuständig für die konkrete Operation
  // F ist entweder DB' oder Reader'[Env, *]
  sealed   trait Free[F[_], A]
  case class Pure[F[_], A](result: A) extends Free[F, A]   // Return1 / Return
  case class Impure[F[_], A](f: F[Free[F, A]]) extends Free[F, A]

  implicit def freeMonad[F[_]](implicit fFunctor: Functor[F]): Monad[Free[F, *]] = new Monad[Free[F, *]] {

    def pure[A](value: A) = Pure[F, A](value)

    def flatMap[A, B](fa: Free[F, A])(next: A => Free[F, B]): Free[F, B] =
      fa match {
        case Pure(result) => next(result)
        case Impure(f) => Impure(f.map(_.flatMap(next)))  // only in IntelliJ an error, but it's compilable
      }

    override def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] = ???
  }

  // ============================

  sealed trait ReaderF[Env, SelfReference]
  case class Ask[Env, SelfReference](callback: Env => SelfReference) extends ReaderF[Env, SelfReference]

//   type IntReaderF[SelfReference] = ReaderF[Int, SelfReference]
//   type IntReader[A] = Free[IntReader, A]
  type Reader[Env, A] = Free[ReaderF[Env, *], A]

  def ask[Env]: Reader[Env, Env] = Impure(Ask(Pure(_)))
 }
