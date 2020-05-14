package example

import cats.Monoid
import cats.instances.int._
import cats.syntax.semigroup._
import cats.instances.option._
object SuperAdder {

  def superAdder(items: List[Int]): Int =
    items.foldRight(0)(_+_)

//  def supperAdderMonoid(items: List[Int]): Int =
//    items.foldRight(Monoid[Int].empty)(_|+|_)

  def superAdderMonoid[A](items: List[A])(implicit m: Monoid[A]): A =
    items.foldRight(m.empty)(_|+|_)

}
