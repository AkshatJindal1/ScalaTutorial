package example

import java.util.Date

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import cats.instances.option._
import cats.instances.list._
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

  //  flatmap
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption
  def divide(a: Int, b: Int): Option[Int] =
    if(b==0) None else Some(a / b)
  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap { aNum =>
      parseInt(bStr).flatMap { bNum =>
        divide(aNum, bNum)
      }
    }

  println(stringDivideBy("6","2"))
  println(stringDivideBy("6","0"))

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatmap
  import cats.syntax.applicative._ // for pure

  println(10.pure[Option])
  println(10.pure[List])

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x*x + y*y))

  //  Can be written using for comprehension
  def sumSquared[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  println(sumSquare(Option(3), Option(4)))
  println(sumSquare(List(1,2,3), List(4,5)))

  println(sumSquared(Option(3), Option(4)))
  println(sumSquared(List(1,2,3), List(4,5)))







}
