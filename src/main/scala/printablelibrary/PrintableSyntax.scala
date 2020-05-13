package printablelibrary

import printablelibrary.Printer.Printable

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]) =
      p.format(value)

    def print(implicit  p: Printable[A]) =
      println(format)
  }

}
