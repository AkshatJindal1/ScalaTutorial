package monadtransforms

import cats.data.{EitherT, OptionT, Writer}
import cats.instances.either._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Main extends App {

  //Composing list and option
  type ListOption[A] = OptionT[List, A]
  type ErrorOr[A] = Either[String, A]
  println(s"result1: $result1")
  type ErrorOrOption[A] = OptionT[ErrorOr, A]
  println(s"result2: $result2")

  println(s"Using flatmap: ${
    result1.flatMap {
      x => result2.map { y => x + y }
    }
  }"
  )
  println(s"Using for: ${
    for {
      x <- result1
      y <- result2
    } yield x + y
  }"
  )
  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]
  type FutureEitherForHttp[A] = EitherT[Future, HttpError, A]
  type Logged[A] = Writer[List[String], A]
  type Response[A] = EitherT[Future, String, A]
  println(s"c: $c")
  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]
  val a = 10.pure[ErrorOrOption]
  println(futureEitherOr)
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))
  println(Await.result(stack, 1.seconds))
  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b
  println(s"errorStack1: ${errorStack1.value}")
  val intermediate = futureEitherOr.value
  println(s"errorStack2: ${errorStack2.value.map(_.getOrElse(-1))}")

  /*
    Reader, Writer ans States are defined in this way:
      type Reader[E, A] = ReaderT[Id, E, A]
      type Writer[W, A] = WriterT[Id, W, A]
      type State[S, A] = StateT[Id, S, A]
  */
  val stack = intermediate.value
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]
  val add1 = addAll("1", "2", "3")
  val add2 = addAll("1", "b", "3")
  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.seconds) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge"
    }
  }

  println(s"Sum of three numbers: $add1")
  println(s"Sum with no numeric: $add2")

  /*
      Exercise 5.4
   */

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield (power1 + power2) > 15

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(bot) => EitherT.right(Future(bot))
      case None => EitherT.left(Future(s"$autobot is unreachable"))
    }

  //  Usage patterns
  sealed abstract class HttpError

  final case class NotFound(item: String) extends HttpError

  final case class BadRequest(msg: String) extends HttpError

  println("\n-----Mission report-------\n")
  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))


}
