package monads

import cats.syntax.either._

import scala.util.Try

object EitherMonad extends App {

  val either1: Either[String, Int] = Right(10)
  val either2: Either[String, Int] = Right(20)

  println(for {
      a <- either1
      b <- either2
    } yield a + b
  )

  val a = 3.asRight[String]
  val b = 4.asRight[String]

  println(
    for {
      x <- a
      y <- b
    } yield x*x + y*y
  )

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if(num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negataive. Stopping!")
      }
    }

  println(countPositive(List(1, 2, 3)))
  println(countPositive(List(1, -2, 3)))

  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchNonFatal(sys.error("This is bad")))

  println(Either.fromTry(Try("foo".toInt)))
  println(Either.fromOption[String, Int](None, "Badness"))

  println("Error".asLeft[Int].getOrElse(0))
  println("Error".asLeft[Int].orElse(2.asRight[String]))

  println(-1.asRight[String].ensure("Must be non-negative!")(_ > 0))

  println("error".asLeft[Int].recover {
    case str: String => -1
  })
  println("error".asLeft[Int].recoverWith {
    case str: String => Right(-1)
  })

  println("foo".asLeft[Int].leftMap(_.reverse))
  println(6.asRight[String].bimap(_.reverse, _ * 7))
  println("bar".asLeft[Int].bimap(_.reverse, _ * 7))
  println(123.asRight[String].swap)

  //  Error handling
  println(
    for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if(b == 0) "DIV0".asLeft[Int] else (a/b).asRight[String]
    } yield c * 100
  )

  sealed trait LoginError extends Product with Serializable

  final case class UserNotFoundError(username: String) extends LoginError
  final case class PasswordIncorrectError(username: String) extends LoginError
  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFoundError(user) =>
        println(s"User not found: $user")
      case PasswordIncorrectError(user) =>
        println(s"Password incorrect for $user")
      case UnexpectedError =>
        println("Unexpected error")
    }

  val result1: LoginResult = User("akshat","password").asRight
  val result2: LoginResult = UserNotFoundError("dave").asLeft

  result1.fold(handleError, println)
  result2.fold(handleError, println)


}
