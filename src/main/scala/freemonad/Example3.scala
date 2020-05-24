package freemonad

import cats._
import cats.free._
import cats.data._

sealed abstract class TeleType[A] extends Product with Serializable

final case class WriteLine(line: String) extends TeleType[Unit]

final case class ReadLine(prompt: String) extends TeleType[String]


object Example3 extends App {

  type TeleTypeT[M[_], A] = FreeT[TeleType, M, A]

  type Log = List[String]

  type TeletypeState[A] = State[List[String], A]

  object TeleTypeOps {

    def writeLine(line: String): TeleTypeT[TeletypeState, Unit] =
      FreeT.liftF[TeleType, TeletypeState, Unit](WriteLine(line))

    def readLine(prompt: String): TeleTypeT[TeletypeState, String] =
      FreeT.liftF[TeleType, TeletypeState, String](ReadLine(prompt))

    def log(s: String): TeleTypeT[TeletypeState, Unit] =
      FreeT.liftT[TeleType, TeletypeState, Unit](State.modify(s :: _))
  }

  def program: TeleTypeT[TeletypeState, Unit] = {
    for {
      userSaid <- TeleTypeOps.readLine("what's up?")
      _ <- TeleTypeOps.log(s"user said: $userSaid")
      _ <- TeleTypeOps.writeLine("thanks, see you soon!")
    } yield ()
  }

  def interpreter = new (TeleType ~> TeletypeState) {
    override def apply[A](fa: TeleType[A]): TeletypeState[A] = {
      fa match {
        case ReadLine(prompt) =>
          println(prompt)
          val userInput = scala.io.StdIn.readLine()
          StateT.pure[Eval, List[String], A](userInput)

        case WriteLine(line) =>
        StateT.pure[Eval, List[String], A](println(line))
      }
    }
  }

  import TeleTypeOps._

  val state = program.foldMap(interpreter)

  val initialState = Nil

  val (stored, _) = state.run(initialState).value

  println(stored)



}
