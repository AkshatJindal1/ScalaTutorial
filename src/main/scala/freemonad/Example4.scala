package freemonad

import cats.free._
import cats._
import cats.data._
import cats.implicits._
import scala.util.Try

sealed trait Ctx[A]
case class Action(value: Int) extends Ctx[Int]


object Example4 extends App {

  def op1: FreeT[Ctx, Option, Int] = FreeT.liftF[Ctx, Option, Int](Action(7))

  def op2: FreeT[Ctx, Option, Int] = FreeT.liftT[Ctx, Option, Int](Some(7))

  def op3: FreeT[Ctx, Option, Int] = FreeT.pure[Ctx, Option, Int](1)

  val onComplete: FreeT[Ctx, Option, Int] =
    for {
      a <- op1
      b <- op2
      c <- op3
    } yield (a + b + c)

  type OptTry[A] = OptionT[Try, A]

  def tryInterpreter: Ctx ~> OptTry = new (Ctx ~> OptTry) {
    override def apply[A](fa: Ctx[A]): OptTry[A] =
      fa match {
        case Action(value) =>
          OptionT.liftF(Try(value))
      }
  }

  def optTryLift: Option ~> OptTry = new (Option ~> OptTry) {
    override def apply[A](fa: Option[A]): OptTry[A] =
      fa match {
        case Some(value) =>
          OptionT(Try(Option(value)))
        case None => OptionT.none
      }
  }

  val hoisted = onComplete.hoist(optTryLift)
  val evaluated = hoisted.foldMap(tryInterpreter)
  val result = evaluated.value

  println(result)

}
