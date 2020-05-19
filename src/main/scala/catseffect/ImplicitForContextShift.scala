package catseffect

import cats.effect.{ContextShift, IO}

import scala.concurrent.ExecutionContext

object ImplicitForContextShift {

  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

}
