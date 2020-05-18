package catseffect

import java.io._
import cats.effect._
import cats.implicits._

import collection.JavaConverters._

object CatsEffectResource extends App {

  val aquire: IO[String] = IO(println("Aquire cats...")) *> IO("cats")

  val release: String => IO[Unit] = _ => IO(println("... release everything"))

  val addDogs: String => IO[String] =
    x => IO(println("... more animals ...")) *> IO.pure(x ++ " and dogs")

  val report: String => IO[String] =
    x => IO(println("... produce weathter report...")) *>
    IO("It's raining " ++ x)

  println(Resource.make(aquire)(release).evalMap(addDogs).use(report).unsafeRunSync)

  def mkResource(s: String) = {
    val acquire = IO(println(s"Aquiring $s")) *> IO.pure(s)

    def release(s: String) = IO(println(s"Releasing $s"))

    Resource.make(acquire)(release)
  }
  val r = for {
    outer <- mkResource("outer")
    inner <- mkResource("inner")
  } yield (outer, inner)

  r.use {
    case (a,b) => IO(println(s"Using $a and $b"))
  }.unsafeRunSync

  val func = IO {
    scala.io.Source.fromString("Hello World")
  }

  Resource.fromAutoCloseable(func).use(source =>
    IO(println(source.mkString))).unsafeRunSync()





}
