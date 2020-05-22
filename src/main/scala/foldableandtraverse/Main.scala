package foldableandtraverse

import cats.data.Validated
import cats.{Applicative, Eval, Foldable, Monoid, Traverse}
import cats.instances.int._
import cats.instances.list._
import cats.instances.option._
import cats.instances.vector._
import cats.instances.string._
import cats.instances.lazyList._
import cats.instances.future._
import cats.syntax.foldable._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {

  //  Ex: 7.1.2 : Reflecting on folds
  List(1,2,3).foldLeft(List.empty[Int])((a,i) => i :: a)  // reverse the list
  List(1,2,3).foldRight(List.empty[Int])((i,a) => i :: a) //  order does not change

  //  Ex: 7.1.3 : Scaf-fold-ing other methods
  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B]) {
      (item, accumulator) => func(item) :: accumulator
    }

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) {
      (item, accumulator) => func(item) ::: accumulator
    }

  def filter[A](list: List[A])(func: A=> Boolean): List[A] =
    list.foldRight(List.empty[A]) {
      (item, accumulator) => if(func(item)) item :: accumulator
      else accumulator
    }
  def sum[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)

  println(map(List(1,2,3))(_ * 2))
  println(flatMap(List(1,2,3))(a => List(a, a*10, a*100)))
  println(filter(List(1,2,3))(_ % 2 == 1))
  println(sum(List(1,2,3)))

  // FOLDABLE IN CATS

  val ints = List(1, 2, 3)
  println(Foldable[List].foldLeft(ints, 0)(_ + _))

  val maybeInt = Option(123)
  println(Foldable[Option].foldLeft(maybeInt, 10)(_ * _))

  def bigData = (1 to 100000).to(LazyList)
  // foldRight in foldable used Eval thus preventing overflow
  val eval: Eval[Long] =
    Foldable[LazyList].
      foldRight(bigData, Eval.now(0L)) { (num, eval) =>
        eval.map(_ + num)
      }
  println(eval.value)

  println(Foldable[Option].nonEmpty(Option(42)))
  println(Foldable[List].find(List(1, 2, 3, 4))(_ % 2 == 0))

  println(Foldable[List].combineAll(List(1, 2, 3)))
  println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))

  val numbers = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val nestedSequences =
    (Foldable[List] compose Foldable[Vector]).combineAll(numbers)

  println(nestedSequences)

  //  Syntax of foldable
  List(1, 2, 3).combineAll
  List(1, 2, 3).foldMap(_.toString)

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.example.com"
  )

  def getUpTime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allUptimes1: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) {
      (accumulator, host) =>
        val uptime = getUpTime(host)
        for {
          accumulator <- accumulator
          uptime <- uptime
        } yield accumulator :+ uptime
    }

  println(Await.result(allUptimes1, 1.seconds))

  //  This can be simplified
  val allUptimes2: Future[List[Int]] =
    Future.traverse(hostnames)(getUpTime)

  println(Await.result(allUptimes2, 1.seconds))

  //  Traversing with Applicatives

  // We can use Applicative in place of the accumulator used above
  List.empty[Int].pure[Future]

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accumulator, item) =>
      (accumulator, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val totalUptime = listTraverse(hostnames)(getUpTime)

  println(Await.result(totalUptime, 1.seconds))

  /*
      Ex: 7.2.2.1: Traversing with vectors
   */
  println(listSequence(List(Vector(1,2), Vector(3,4))))
  println(listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6))))

  /*
      Ex: 7.2.2.2: Traversing with options
   */
  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

  println(process(List(2, 4, 6)))
  println(process(List(1, 2, 3)))

  /*
      Ex: 7.2.2.3: Traversing with Validated
   */
  type ErrorOr[A] = Validated[List[String], A]

  def validatedProcess(inputs: List[Int]): ErrorOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) Validated.valid(n)
      else Validated.invalid(List(s"$n is not even"))
    }

  println(validatedProcess(List(2, 4, 6)))
  println(validatedProcess(List(1, 2, 3)))

  //  TRAVERSE WITH CATS

  val totalTimeUp: Future[List[Int]] =
    Traverse[List].traverse(hostnames)(getUpTime)

  println(Await.result(totalTimeUp, 1.seconds))

  val number = List(Future(1), Future(2), Future(3))
  val numbers2: Future[List[Int]] =
    Traverse[List].sequence(number)
  println(Await.result(numbers2, 1.seconds))



}
