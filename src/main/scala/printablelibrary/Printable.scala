package printablelibrary

import printablelibrary.Printer.Printable

object Printable {

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(format(value))

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

}
