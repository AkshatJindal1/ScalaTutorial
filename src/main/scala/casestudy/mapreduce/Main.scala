package casestudy.mapreduce

import cats.Monoid
import cats.instances.int._
import cats.instances.string._
import cats.instances.future._
import cats.instances.vector._

import cats.syntax.semigroup._
import cats.syntax.foldable._
import cats.syntax.traverse._

import cats.Foldable
import cats.Traverse

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object Main extends App {

  def foldMap[A, B: Monoid](seq: Vector[A])(func: A => B): B =
    seq.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  foldMap(Vector(1, 2, 3))(identity)            // Output: 6
  foldMap(Vector(1, 2, 3))(_.toString + "! ")   // Output: "1! 2! 3! "

  println(Runtime.getRuntime.availableProcessors)

  def parallelFoldMap[A, B: Monoid](seq: Vector[A])(func: A => B): Future[B] = {

    val cpus = Runtime.getRuntime.availableProcessors
    val batchSize = (seq.size.toFloat / cpus).ceil.toInt

    // creating batches per CPU
    val batches: Iterator[Vector[A]] = seq.grouped(batchSize)

    // Create a future to fold each batch
    val futures: Iterator[Future[B]] =
      batches.map { batch =>
        Future(foldMap(batch)(func))
      }
    // foldmap over batches to get final result
    Future.sequence(futures) map { iterable =>
      iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }
  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)

//  Await.result(result, 1.second)

  def parallelFoldMapWithCats[A, B: Monoid](seq: Vector[A])(func: A => B): Future[B] = {

    val cpus = Runtime.getRuntime.availableProcessors
    val batchSize = (seq.size.toFloat / cpus).ceil.toInt

    seq
      .grouped(batchSize)
      .toVector
      .traverse( batch => Future(batch.toVector.foldMap(func)))
      .map(_.combineAll)
  }

  val future: Future[Int] =
    parallelFoldMapWithCats((1 to 1000000).toVector)(_ * 1000)

  Await.result(future, 1.second)



}
