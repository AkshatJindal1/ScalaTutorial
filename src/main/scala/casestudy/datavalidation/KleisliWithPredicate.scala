package casestudy.datavalidation

import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.instances.either._
import cats.instances.list._
import cats.syntax.apply._

object KleisliWithPredicate extends App {

  type Errors = NonEmptyList[String]
  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

  def error(s: String):NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(field: String)(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$field: Must be longer than $n characters"),
      str => str.size > n
    )

  def alphanumeric(field: String): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$field: Must be all alphanumeric"),
      str => str.forall(_.isLetterOrDigit)
    )

  def contains(field: String)(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$field: Must contain the character $char"),
      str => str.contains(char)
    )

  def containsOnce(field: String)(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"$field: Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1
    )

  val checkUsername: Check[String, String] =
    checkPred(longerThan("username")(3) and alphanumeric("username"))

  val splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case othere => Left(error("Must contain a single @ character"))
    })

  val checkLeft: Check[String, String] =
    checkPred(longerThan("email")(0))

  val checkRight: Check[String, String] =
    checkPred(longerThan("email")(3) and contains("email")('.'))

  val joinEmail: Check[(String, String), String] =
    check {
      case(l,r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkEmail: Check[String, String] =
    splitEmail andThen joinEmail

  def createUser(username: String, email: String): Either[Errors, User] =
    (checkUsername.run(username), checkEmail.run(email)).mapN(User)

  println(createUser("Akshat","aks@jin.com"))
  println(createUser("","ak@ak"))


}
