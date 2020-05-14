package functors

import cats.instances.function._
import cats.syntax.functor._

object FunctionComposition extends App {

  val func1: Int => Double =
    (x: Int) => x.toDouble
  /*
      Can also we written as :
        1.  val func1 = (x: Int) => x.toDouble
        2.  val func1: Int => Double = x => x.toDouble
   */

  val func2: Double => Double =
    (y: Double) => y * 2

  println((func1 map func2)(1))

  println((func1 andThen func2)(1))

  println(func2(func1(1)))

  def func3(x: Int): Double =
    x.toDouble

  def func4(y: Double): Double =
    y * 2

//  println((func3 map func4)(1)) => this doesn't work



}
