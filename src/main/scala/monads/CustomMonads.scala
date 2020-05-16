package monads

import cats.Monad
import datatype.{Branch, Leaf, Tree}

import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.annotation.tailrec

object CustomMonads extends App {

  val optionMonad = new Monad[Option] {
    override def flatMap[A,B](opt: Option[A])(fn: A => Option[B]): Option[B] =
      opt flatMap fn

    override def pure[A](opt: A): Option[A] =
      Some(opt)

    @tailrec
    override def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }

  /*
      Ex 4.10.1 : Monads for tree data type that was last defined
   */

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](value: A): Tree[A] =
      Leaf(value)

    override def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(left, right) => Branch(
          flatMap(left)(fn),
          flatMap(right)(fn))

        case Leaf(value) => fn(value)
    }

    override def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(fn(a)) {
        case Left(value) => tailRecM(value)(fn)
        case Right(value) => Leaf(value)
      }
  }

  val tree = Tree.branch(Tree.leaf(100), Tree.leaf(200))

  println(tree.flatMap(x => Tree.branch(Tree.leaf(x - 1), Tree.leaf(x + 1))))

  println(
    for {
      a <- tree
      b <- Tree.branch(Tree.leaf(a - 10), Tree.leaf(a + 10))
      c <- Tree.branch(Tree.leaf(b - 1), Tree.leaf(b + 1))
    } yield c
  )


}
