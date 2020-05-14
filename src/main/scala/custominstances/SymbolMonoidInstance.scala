package custominstances

import cats.Monoid
import cats.instances.string._
import cats.syntax.invariant._

object SymbolMonoidInstance {

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)

}
