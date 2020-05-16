package monads

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

import cats.Id

object IdentityMonad extends App {

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]) =
    for {
      x <- a
      y <- b
    } yield  x*x + y*y

  // sumSquare(3,4) wont work as we have nto defined for type direct
   val x: Id[Int] = 12
  println(x)

  println(sumSquare(3: Id[Int], 4: Id[Int]))

  val a = Monad[Id].pure(3)
  val b = Monad[Id].flatMap(a)(_ + 1)

  println(
    for {
      x <- a
      y <- b
    } yield x + y
  )

  /*
      4.3.1 : Monadic secret identities
   */

  def pure[A](value: A): Id[A] = value

  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] =
    func(value)

  def map[A, B](value: Id[A])(func: A => B): Id[B] =
    func(value)

}
