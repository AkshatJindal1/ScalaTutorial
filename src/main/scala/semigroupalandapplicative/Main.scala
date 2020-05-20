package semigroupalandapplicative

import cats.data.Validated
import cats.{Monad, Monoid, Semigroupal}
import cats.instances.option._
import cats.instances.int._
import cats.instances.string._
import cats.instances.list._
import cats.instances.invariant._
import cats.instances.future._
import cats.instances.either._
import cats.instances.vector._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.validated._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.either._

object Main extends App {

  println(Semigroupal[Option].product(Some(123), Some("abc")))  //  if both are some we get tuple of the values
  println(Semigroupal[Option].product(Some(123), None)) // if any one is none, overall is none

  println(Semigroupal.tuple3(Option(1), Option(2), Option(3)))    // this function goes from tuple2 to tupple22
  println(Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]))

  val optionSum = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  println(s"Sum of three options: $optionSum")

  println(s"Sum of options with a None: ${Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)}")

  //  Apply syntax
  //  1. tupled
  println((Option(123), Option("abc")).tupled)

  println(s"Syntax for tuples: ${(Option(123), Option("abc"),Option(true)).tupled}")

  //  2. mapN
  case class Cat(name: String, born: Int, color: String)

  val catGarfield = (
    Option("Garfield"), Option(1998), Option("Orange and Black")
  ).mapN(Cat.apply)
  println(s"Garfield details $catGarfield")

  //  The syntax is type checked
  val add: (Int, Int) => Int = (a, b) => a + b
//  println((Option(1), Option(2), Option(3)).mapN(add)) will give error as add takes 2 params but we have supplied 3
//  println((Option("cat"),Option(true)).mapN(add))  in this case the types are not same

  println((Option(1),Option(2)).mapN(add))

  case class CatAndFood(name: String, yearOfBirth: Int, favouriteFood: List[String])

  val tupleToCat: (String, Int, List[String]) => CatAndFood = CatAndFood.apply _

  val catToTuple: CatAndFood => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favouriteFood)

  implicit val catMonoid: Monoid[CatAndFood] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  val garfield = CatAndFood("Garfield", 1978 ,List("fish"))
  val healthcliff = CatAndFood("Healthcliff",1988, List("Junk food"))

  println(s"Sum of cats: ${garfield |+| healthcliff}")

  //  Semigroupal to different types
  val futurePair = Semigroupal[Future].
    product(Future("Hello"), Future(123))
  println(Await.result(futurePair, 1.seconds))

  val futureCat = (
    Future("Garfield"),
    Future(1988),
    Future(List("Fish"))
  ).mapN(CatAndFood.apply)
  println(Await.result(futureCat, 1.seconds))

  //  Combining list with semigroupal workes ina different way
  val listSemigrooupal = Semigroupal[List].product(List(1,2), List(3,4))
  println(s"Semigroupal for list: $listSemigrooupal")

  // Semigroupal on either acts as fail fast and does not accumulate all the errors
  type ErrorOr[A] = Either[Vector[String], A]
  val eitherSemigroupal = Semigroupal[ErrorOr].
    product(Left(Vector("Error 1")), Left(Vector("Error 2")))
  println(s"Semigroupal of either $eitherSemigroupal")

  /*
      The reason for this behaviour is that both List and Either are Monads. And the product
      function in them is made using maps and flatmaps which have this inherent property. The
      case is same for Future also
   */

  /*
      Exercise 6.3.1.1 : Implement product in terms of flatMap
   */

  def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)


  /*
        Validated data type from Cats. This gives us option ot accumulate errors
        which was not possible in either and lists.
        It is an instance of Semigroupa but not Monad
   */

  type AllErrorsOr[A] = Validated[List[String], A]

  val validatedSemigroupal = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )
  println(s"Semigroupal of validated: $validatedSemigroupal")

  //  Subtypes of Validated: Valid and Invalid
  val v = Validated.Valid(123)
  val i = Validated.Invalid(List("Badness"))
  println(s"Valid validated sub type: $v")
  println(s"Invalid validated sub type: $i")

  //  Smart constructors of Validated: valid, invalid
  val v1 = Validated.valid[List[String], Int](123)
  val i1 = Validated.invalid[List[String], Int](List("Badness"))
  println(s"Valid validated constructor: $v1")
  println(s"Invalid validated constructor: $i1")

  //  Syntax for validated
  val v2 = 123.valid[List[String]]
  val i2 = List("Badness").invalid[Int]
  println(s"Valid validated syntax: $v2")
  println(s"Invalid validated syntax: $i2")

  //  Pure and raiseError from cats.syntax.applicative and cats.syntax.applicativeError
  type ValidatedErrorOr[A] = Validated[List[String], A]
  val v3 = 123.pure[ValidatedErrorOr]
  val i3 = List("Badness").raiseError[ValidatedErrorOr, Int]
  println(s"Validated pure: $v3")
  println(s"Validated raiseError: $i3")

  //  Helper methods to create instances of Validates from Different sources
  val er1 = Validated.catchOnly[NumberFormatException]("foo".toInt)
  val er2 = Validated.catchNonFatal(sys.error("Badness"))
  val er3 = Validated.fromTry(scala.util.Try("foo".toInt))
  val er4 = Validated.fromEither[String, Int](Left("Badness"))
  val er5 = Validated.fromOption[String, Int](None, "Badness")
  println(s"Exception handelling : $er1")
  println(s"Error handelling     : $er2")
  println(s"From try             : $er3")
  println(s"From either          : $er4")
  println(s"From option          : $er5")

  //  Combining instances of Validated
  type ValidatedAllErrorsOr[A] = Validated[String, A]

  println(
    (
      Vector(404).invalid[Int],
      Vector(500).invalid[Int]
    ).tupled
  )

  //  Methods of Validated
  println(123.valid.map(_ * 100))
  println("?".invalid.leftMap(_.toString))
  println(123.valid[String].bimap(_ + "!", _ * 100))
  println("?".invalid[Int].bimap(_ + "!", _ * 100))

  //  We can flatMap on Validates as it is not Monad
  //  But we have similar andThen
  val andThenExample = 32.valid.andThen { a =>
    10.valid.map { b =>
      a + b
    }
  }
  println(andThenExample)

  /*
      Ex: 6.4.4 : Form validation
   */

  case class User(name: String, age: Int)


  def getValue(field: String)(mp: Map[String, String]): Either[List[String], String] =
    mp.get(field).toRight(List(s"$field field not specified"))

  def parseInt(value: String): Either[List[String], Int] =
    Either.catchOnly[NumberFormatException](value.toInt).
      leftMap(_ => List("Not a number"))

  def nonBlank(field: String)(value: String): Either[List[String], String] =
    Right(value).ensure(List(s"$field value is empty"))(_.nonEmpty)

  def nonNegative(value: Int): Either[List[String], Int] =
    Right(value).ensure(List("Age is negative"))(_ >=0)

  def readName(mp: Map[String, String]): Either[List[String], String] =
    getValue("name")(mp).flatMap(nonBlank("name"))

  def readAge(mp: Map[String, String]): Either[List[String], Int] =
    getValue("age")(mp).
      flatMap(nonBlank("age")).
      flatMap(parseInt).
      flatMap(nonNegative)

  def readUser(user: Map[String, String]): Validated[List[String], User] =
    (
      readName(user).toValidated,
      readAge(user).toValidated
    ).mapN(User.apply)
  println("\n-------------- FORM VALIDATION ---------------")
  println(readUser(Map("name"-> "Akshat", "age"-> "37")))
  println(readUser(Map("age" -> "-1")))
  println(readUser(Map("name"-> "", "age"-> "-54")))






}
