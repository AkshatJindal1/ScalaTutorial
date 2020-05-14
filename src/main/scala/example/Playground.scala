package example

import java.util.Date

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._
import custominstances.CatShowInstances._
import custominstances.CatsEqInstances._
import datatype.{Box, Cat, Order}
import cats.Monoid
import cats.Semigroup
import cats.syntax.semigroup._
import custominstances.OrderMonoidInstances._
import codec.CodecInstances._
import codec.Codec

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

  println(new Date().show)
  println(Cat("abc", 2,"abc").show)

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  println(cat1 === cat2)
  println(Option(cat1) === Option(cat1))
  println(cat1.some === cat2.some)

  //  Monoid
  println(Monoid[String].combine("Hi ","there"))
  println(Monoid[String].empty)
  println(Semigroup[String].combine("Hi ","Akshat"))
  println(Monoid[Int].combine(12,24))
  println(Monoid[Option[Int]].combine(Option(12), Option(24)))

  //  Monoid Syntax
  println("Hi " |+| "there")

  val orderList = List(Order(12, 6), Order(14,10), Order(124, 32))
  println(s"Total order and sum is ${SuperAdder.superAdderMonoid(orderList)}")

  //  Codec
  println(Codec.encode(12))
  println(Codec.decode[Double]("12"))
  println(Codec.encode(Box(12)))
  println(Codec.decode[Box[Boolean]]("true"))






}
