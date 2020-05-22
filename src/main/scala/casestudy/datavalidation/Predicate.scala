package casestudy.datavalidation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.semigroup._
import cats.syntax.apply._
import cats.syntax.validated._

sealed trait Predicate[E, A] {
  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) => func(value)

      case And(left, right) =>
        (left(value), right(value)).mapN((_, _) => value)

      case Or(left, right) =>
        left(value) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) =>
            right(value) match {
              case Valid(a) => Valid(a)
              case Invalid(e2)  => Invalid(e1 |+| e2)
            }
        }
    }

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    (a: A) => this(a).toEither

}

object Predicate {

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E,A])
    extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
    extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A])
    extends Predicate[E, A]

  def apply[E, A](func: A => Validated[E, A]): Predicate[E, A] =
    Pure(func)

  def lift[E, A](err: E, func: A => Boolean): Predicate[E, A] =
    Pure(a => if(func(a)) a.valid else err.invalid)

}


