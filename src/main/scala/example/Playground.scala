package example

import cats.Show
import cats.instances.int._
import cats.instances.string._

import cats.syntax.show._ // show interface syntax

object Playground extends App {

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("abc")

  println(intAsString)
  println(stringAsString)

  //Interface syntax
  val showInt2 = 123.show
  val showString2 = "abc".show
  println(showInt2)
  println(showString2)

}
