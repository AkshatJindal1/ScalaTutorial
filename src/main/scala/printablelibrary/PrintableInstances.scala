package printablelibrary

import datatype.Cat
import printablelibrary.Printer.Printable

object PrintableInstances {

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      override def format(value: String): String = value
    }

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      override def format(value: Int): String = value.toString
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



}
