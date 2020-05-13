package monoidsandsemigroups

import monoidsandsemigroups.MonoidsAndSemigroups.Monoid

object MonoidSyntax {

  implicit class MonoidCombineOps[A](x: A) {
    def combine(y:A)(implicit m: Monoid[A]): A =
      m.combine(x,y)
  }


}
