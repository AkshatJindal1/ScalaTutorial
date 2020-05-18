package catseffect

import java.util.concurrent.ScheduledExecutorService
import cats.effect.IO
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object CatsEffectIO extends App {

  val ioa = IO {println("Hi!")}

  val prog: IO[Unit] =
    for {
      _ <- ioa
      _ <- ioa
    } yield ()

  prog.unsafeRunSync()

  /*
      IO is trampolined in its flatMap evaluation.
      This makes call of flatMap in a recursive function stack safe
   */
  def fib(n: Int, a: BigInt = 0, b: BigInt = 1): IO[BigInt] =
    IO(a+b).flatMap { b2 =>
      if(n > 0) fib(n-1, b, b2)
      else IO.pure(a)
    }
  println(s"100th fibonacci number is: ${fib(100).unsafeRunSync()}")
  // IO.pure is eagery evaluated with the parameters passed, so we shoudl not do this
  IO.pure(println("THIS IS WRONG!!!"))

  // Syncronous execution
  def putStrLn(value: String) = IO(println(value))
  def readLn = IO(scala.io.StdIn.readLine)

  /*
     Greeting function:
          val greet =
            for {
              _ <- putStrLn("What is your name")
              n <- readLn
              _ <- putStrLn(s"Hello, $n")
            } yield ()
  greet.unsafeRunSync()
  */

  //  Asynchronous effects:
  //  1. IO.async
  def convert[A](fa: => Future[A])(implicit ec: ExecutionContext): IO[A] =
    IO.async { cb =>
      fa.onComplete {
        case Success(a) => cb(Right(a))
        case Failure(e) => cb(Left(e))
      }
    }
  //  2. IO.cancelable
  def delayedTick(d: FiniteDuration)(implicit sc: ScheduledExecutorService): IO[Unit] = {
    IO.cancelable { cb =>
      val r = new Runnable {
        override def run(): Unit = cb(Right(()))
      }
      val f = sc.schedule(r,d.length, d.unit)
      IO(f.cancel(false))
    }
  }

  // IO.suspend
  def fib2(n: Int, a: BigInt = 0, b: BigInt= 1): IO[BigInt] =
    IO.suspend {
      if(n > 0) fib(n-1, b, a + b)
      else IO.pure(a)
    }

  println(fib2(100).unsafeRunSync())





}
