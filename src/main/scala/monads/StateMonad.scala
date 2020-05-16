package monads

import cats.data.State
import State._
import cats.syntax.applicative._

object StateMonad extends App {

  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }
  println(a)

  println(a.run(10).value)
  println(a.runS(10).value)
  println(a.runA(10).value)

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }
  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield(a, b)

  println(both.run(20).value)

  val getDemo = State.get[Int]
  println(s"Example of get: ${getDemo.run(10).value}")
  val setDemo = State.set[Int](30)
  println(s"Example of set: ${setDemo.run(10).value}")
  val pureDemo = State.pure[Int, String]("Result")
  println(s"Example of pure: ${pureDemo.run(10).value}")
//  val inspectDemo = State.inspect[Int, String](_ + "!")
//  println(s"Example of inspect: ${inspectDemo.run(10).value}")
  val modifyDemo = State.modify[Int]( _ + 1)
  println(s"Example of modify: ${modifyDemo.run(10).value}")

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a+1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield(a,b,c)
  println(program.run(1).value)

  /*
      Ex 4.9.3: Post order calculator
   */

  type CalcState[A] = State[List[Int], A]

  def operator(function: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = function(a,b)
        (ans :: tail, ans)

      case _ =>
        sys.error("Fail")
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }


  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  println(evalOne("42").runA(Nil).value)

  val prog: CalcState[Int] = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  println(prog.runA(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  println(evalAll(List("1","2","+","3","*")).runA(Nil).value)

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  println(evalInput("1 2 + 3 4 + *"))

}
