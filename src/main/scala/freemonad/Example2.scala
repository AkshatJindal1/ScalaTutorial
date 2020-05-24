package freemonad

import cats.data.EitherK
import cats.free.Free
import cats.{Id, InjectK, ~>}

import scala.collection.mutable.ListBuffer

sealed trait Interact[A]
case class Ask(prompt: String) extends Interact[String]
case class Tell(msg: String) extends Interact[Unit]

sealed trait DataOp[A]
case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]

class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
  def tell(msg: String): Free[F, Unit] =
    Free.inject[Interact, F](Tell(msg))

  def ask(promp: String): Free[F, String] =
    Free.inject[Interact, F](Ask(promp))
}

object Interacts {
  implicit def interacts[F[_]]
      (implicit I: InjectK[Interact, F]): Interacts[F] =
    new Interacts[F]
}

class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
  def addCat(a: String): Free[F, Unit] =
    Free.inject[DataOp, F](AddCat(a))

  def getAllCats: Free[F, List[String]] =
    Free.inject[DataOp, F](GetAllCats())
}

object DataSource {
  implicit def dataSource[F[_]]
      (implicit I: InjectK[DataOp, F]): DataSource[F] =
    new DataSource[F]
}

object ConsoleCatsInterpreter extends (Interact ~> Id) {
  def apply[A](i: Interact[A]) =
    i match {
      case Ask(prompt) =>
        println(prompt)
        scala.io.StdIn.readLine()
      case Tell(msg) =>
      println(msg)
    }
}

object ImMemoryDatasourceInterpreter extends (DataOp ~> Id) {

  private[this] val memDataSet = new ListBuffer[String]

  def apply[A](fa: DataOp[A]) =
    fa match {
      case AddCat(a) =>
        memDataSet.append(a)
        ()
      case GetAllCats() =>
        memDataSet.toList
    }
}

object Example2 extends App {

  type CatsApp[A] = EitherK[DataOp, Interact, A]

  def program(implicit I: Interacts[CatsApp],
              D: DataSource[CatsApp]): Free[CatsApp, Unit] = {

    import I._, D._

    for {
      cat <- ask("What's the kitty's name?")
      _ <- addCat(cat)
      cats <- getAllCats
      _ <- tell(cats.toString)
    } yield ()
  }

  val interpreter: CatsApp ~> Id =
    ImMemoryDatasourceInterpreter or ConsoleCatsInterpreter

  import DataSource._, Interacts._

  val evaled: Unit = program.foldMap(interpreter)
  println(evaled)

}
