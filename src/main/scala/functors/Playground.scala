package functors

import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.instances.function._
import cats.syntax.functor._

object Playground extends App {

  val list1 = List(1, 2, 3)

  val list2 = Functor[List].map(list1)(_ * 2)
  val list3 = (Functor[List] map list1)(_ * 2)  //  same as list2
  println(list2)
  println(list3)

  val option1 = Option(420)

  val option2 = Functor[Option].map(option1)(_.toString)
  val option3 = (Functor[Option] map option1)(_.toString)
  println(s"$option1 \n$option2")

  /*
        Lifting a function to it higher type:
          A => B becomes F[A] => F[B]
   */

  val func = (x: Int) => x+1

  val liftedFunc = Functor[Option].lift(func)
  println(liftedFunc(Option(3)))

  //  Functor syntax
  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => a.toString + "!"
  val func4 = func1.map(func2).map(func3)
  println(func4(123))

  def doMath[F[_]]
        (start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  println(doMath(Option(20)))
  println(doMath(List(1, 2, 3)))









}
