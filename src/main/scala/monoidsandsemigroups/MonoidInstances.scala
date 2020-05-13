package monoidsandsemigroups

import monoidsandsemigroups.MonoidsAndSemigroups.Monoid

object MonoidInstances {

  implicit val booleanAnd: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit val booleanOr: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }
  // We use def instead of val so as to take care of different data types that can make a set
  implicit def setUnion[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    }

//  implicit def setIntersection[A]: Monoid[Set[A]] =
//    new Monoid[Set[A]] {
//      override def empty: Set[A] = ??? universal set maybe
//
//      override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
//    }




}
