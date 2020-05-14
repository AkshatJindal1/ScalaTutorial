package printablelibrary

import cats.kernel.Monoid
import datatype.{Box, Cat}
import printablelibrary.Printer.Printable

object PrintableInstances {

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      override def format(value: String): String =
        s"converted to string: $value"
    }

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      override def format(value: Boolean): String =
        if(value) "yes" else "no"
    }

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
      override def format(value: Cat): String = {
//        s"${value.name} is a ${value.age} year-old ${value.color} cat."
        s"${Printable.format(value.name)} is a " +
          s"${Printable.format(value.age)} year-old " +
          s"${Printable.format(value.color)} cat."

      }
    }

  implicit def optionPrintable[A](implicit p: Printable[A]): Printable[Option[A]] =
    new Printable[Option[A]] {
      override def format(option: Option[A]): String =
        option match {
          case Some(aValue) => p.format(aValue)
          case None => ""
        }
    }

  /*
      implicit def optionPrintable[A](implicit p: Printable[A]) =
        p.contramap[Option[A]](_.get)

        This will fail in case option is None as we cannot use getOrDefault
        to provide default value as we do not know type of A

   */


  implicit def boxPrintable[A](implicit p: Printable[A]) =
    p.contramap[Box[A]](_.value)

}
