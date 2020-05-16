package monads

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object WriterMonad extends App {

  println(Writer(Vector(
    "I am good",
    "I am bad"
  ), 1859))

  type Logged[A] = Writer[Vector[String], A]

  //  syntactical way of writing
  println(123.pure[Logged])                         //  only result
  println(Vector("msg1", "msg2","msg3").tell)       //  only logs
  println(123.writer(Vector("msg1","msg2")))        //  both logs and result

  //extracting logs and result from writer
  val a = 123.writer(Vector("msg1","msg2"))
  val aResult: Int = a.value                        //  extracting result
  val aLog: Vector[String] = a.written              //  extracting logs
  println(aResult)
  println(aLog)
  val (log, result) = a.run                         //  extracting both of them together

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  println(writer1.run)
  println(writer1.mapWritten(_.map(_.toUpperCase)).run)

  //  transformation on both log and result together
  val writer2 = writer1.bimap(
    log => log.map(_.toUpperCase), res => res * 100
  )
  println(writer2.run)

  val writer3 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }
  println(writer3.run)

  // reset the logs
  val writer4 = writer1.reset
  println(writer4.run)

  //  swap logs and results
  val writer5 = writer1.swap
  println(writer5.run)

  /*
      Ex 4.7.3: Logging for parallel process

        def factorial(n: Int): Int = {
          val ans = slowly(if(n==0) 1 else n * factorial(n-1))
          println(s"fact $n $ans")
          ans
        }

   */
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)


  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if(n == 0) 1.pure[Logged] else slowly(factorial(n-1).map(_ * n)) // this does the calculation of ans
      _ <- Vector(s"fact $n $ans").tell   //  this does setting log. Because of flatMap it is getting appended to same logs vector
    } yield ans

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

   val Vector((logA, ansA), (logB, ansB)) =
     Await.result(Future.sequence(Vector(
      Future(factorial(3).run),
      Future(factorial(4).run)
    )), 5.seconds)

  println(logA)
  println(ansA)
  println(logB)
  println(ansB)

  val writer6 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a","b","c").tell
  } yield a
  println(writer6.run)




}
