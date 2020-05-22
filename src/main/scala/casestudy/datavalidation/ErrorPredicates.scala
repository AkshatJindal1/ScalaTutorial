package casestudy.datavalidation

import cats.data.{NonEmptyList, Validated}
import cats.syntax.apply._
import cats.syntax.validated._

object ErrorPredicates extends App {

  type Errors = NonEmptyList[String]

  def error(s: String): Errors =
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

  val checkUserName: Check[Errors, String, String] =
    Check(longerThan("username")(3) and alphanumeric("username"))

  val splitEmail: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) => (name, domain).validNel[String]

      case other => "Must contain a single @ character".
        invalidNel[(String, String)]
    })

  val checkLeft: Check[Errors, String, String] =
    Check(longerThan("email")(0))

  val checkRight: Check[Errors, String, String] =
    Check(longerThan("email")(3) and contains("email")('.'))

  val joinEmail: Check[Errors, (String, String), String] =
    Check { case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkEmail: Check[Errors, String, String] =
    splitEmail andThen joinEmail

  def createUser(username: String, email: String): Validated[Errors, User] =
    (
      checkUserName(username),
      checkEmail(email)
    ).mapN(User)


  println(createUser("Akshat","aks@jin.com"))
  println(createUser("#4","aks@ab.cd"))

}
