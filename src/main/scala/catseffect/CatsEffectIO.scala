package catseffect

import java.io._
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.atomic.AtomicBoolean

import cats.effect._
import cats.implicits._
import catseffect.ImplicitForContextShift._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object CatsEffectIO extends App {

  val ioa = IO {
    println("Hi!")
  }

  val prog: IO[Unit] =
    for {
      _ <- ioa
      _ <- ioa
    } yield ()

  prog.unsafeRunSync()


  IO.pure(25).flatMap(n => IO(println(s"Number is $n"))).unsafeRunSync

  /*
      IO is trampolined in its flatMap evaluation.
      This makes call of flatMap in a recursive function stack safe
   */
  def fib(n: Int, a: BigInt = 0, b: BigInt = 1): IO[BigInt] =
    IO(a + b).flatMap { b2 =>
      if (n > 0) fib(n - 1, b, b2)
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

  //  Non terminating IO that is NOT cancellable
  val notCancelable: IO[Int] = retryUntilRight(IO(Left(0)))


  // IO.suspend
  def fib2(n: Int, a: BigInt = 0, b: BigInt = 1): IO[BigInt] =
    IO.suspend {
      if (n > 0) fib(n - 1, b, a + b)
      else IO.pure(a)
    }

  println(s"200th fibonacci number is: ${fib2(200).unsafeRunSync()}")

  //  def fib3(n: Int, a: BigInt=0, b: BigInt = 1)(implicit cs: ContextShift[IO]): IO[BigInt] =
  //    IO.suspend {
  //      if(n == 0) IO.pure(a) else {
  //        val next = fib3(n-1, b, a + b)
  //        // Every 100th cycle, introduce a logical thread fork
  //        if(n % 100 == 0) cs.shift *> next else next
  //      }
  //    }
  //  println(s"300th fibonacci number is: ${fib3(300).unsafeRunSync}")
  // non-terminating IO that is cancellable because it is an
  // async created by IO.shift before flatmap chain
  val cancelable: IO[Int] = IO.shift *> retryUntilRight(IO(Left(0)))
  //  Concurrent start + cancel
  val launchMissiles: IO[Unit] = IO.raiseError(new Exception("boom!"))
  val runToBunker = IO(println("To the bunker!!"))

  //  Building cancellable IO tasks
  //  example of delayed tick

  //  2. IO.cancelable
  def delayedTick(d: FiniteDuration)(implicit sc: ScheduledExecutorService): IO[Unit] = {
    IO.cancelable { cb =>
      val r = new Runnable {
        override def run(): Unit = cb(Right(()))
      }
      val f = sc.schedule(r, d.length, d.unit)
      IO(f.cancel(false)).void
    }
  }

  def retryUntilRight[A, B](io: IO[Either[A, B]]): IO[B] = {
    io.flatMap {
      case Right(b) => IO.pure(b)
      case Left(_) => retryUntilRight(io)
    }
  }

  def readLine(in: BufferedReader)(implicit ec: ExecutionContext) =
    IO.cancelable[String] { cb =>
      val isActive = new AtomicBoolean(true)
      ec.execute { () =>
        if (isActive.getAndSet(false)) {
          try cb(Right(in.readLine()))
          catch {
            case NonFatal(e) => cb(Left(e))
          }
        }
      }
      IO {
        if (isActive.getAndSet(false))
          in.close()
      }.void
    }

  for {
    fiber <- launchMissiles.start
    _ <- runToBunker.handleErrorWith { error => fiber.cancel *> IO.raiseError(error) }
    aftermath <- fiber.join
  } yield aftermath


}
