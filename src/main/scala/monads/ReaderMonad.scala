package monads

import cats.data.Reader

object ReaderMonad extends App {

  case class Cat(name: String, favouriteFood: String)

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  println(catName.run(Cat("Billu", "fish")))

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello $name")

  println(greetKitty.run(Cat("Kitty","milk")))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favouriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."

  println(greetAndFeed(Cat("Kitty", "milk")))

  /*
      Ex: 4.8.3
   */
  case class Db( usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      flag <- checkPassword(username.getOrElse(""), password)
    } yield flag

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "abcde",
    "kate" -> "fghij",
    "margo" -> "klmno"
  )

  val db = Db(users, passwords)

  println(checkLogin(4, "abcde").run(db))

}
