package monads

import cats.Eval

object EvalMonod extends App {

  val now = Eval.now(math.random + 1000)      //  this is same as val -> value calculated right now
  val later = Eval.later(math.random+ 2000)   //  this is like lazy val -> value is calculated when the first tie it is used and then stored
  val always = Eval.always(math.random+ 3000) //  this si like def -> value is calculated every time is is used

  println(s"now for two times: ${now.value} , ${now.value}")
  println(s"later for two times: ${later.value} , ${later.value}")
  println(s"always for two times: ${always.value} , ${always.value}")

  //  map in eval
  println("\n  map function in eval: ")
  val greeting = Eval.always {
    println("Step 1")
    "Hello"
  }.map{ str =>
    println("Step2")
    s"$str world"
  }
  println(greeting.value)

  val ans = for {
    a <- Eval.now {println("Calculating A"); 40}
    b <- Eval.always {println("Calculating B"); 2}
  } yield {
    println("Adding A and B")
    a + b
  }
  println(ans)
  println()
  println(s"${ans.value}")

  //  memoize method is used to memoize call upto point
  //  in chain where moize is called and after that it is always calculated
  val saying = Eval.
    always { println("Step 1"); "The cat"}.
    map { str => println("Step 2"); s"$str sat on" }.
    memoize.
    map { str => println("Step 3"); s"$str the mat" }

  println()
  println(s"Calling saying 1st time: ${saying.value}")
  println(s"Calling saying 2nd time: ${saying.value}")

  //  eval.defer method
  def fact1(n: BigInt): Eval[BigInt] =
    if(n==1) Eval.now(n)
    else fact1(n-1).map(_ * n)

  //  this will give stack over flow for very big ints
  //  defer method takes an existing instance of Eval and defers its evaluation
  //  the defer method is trampolined like a map and flatMap so it can be used to
  //  make operations stack safe
  def fact2(n: BigInt): Eval[BigInt] =
    if(n == 1) Eval.now(n)
    else Eval.defer(fact2(n-1).map(_ * n))
//  println(fact2(50000).value) -> this will work

  /*
      Ex: 4.6.5: Make this stack safe

      def foldRight[A, B](as: List[A], acc: B)(fn: (A,B) => B): B =
        as match {
          case head :: tail =>
            fn(head, foldRight(tail, acc)(fn))
          case Nil => acc
        }
   */

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => acc
    }
  def foldRight[A, B](as: List[A], acc: B)(fn: (A,B) => B): B =
    foldRightEval(as, Eval.now(acc)) {
      (a, b) => b.map(fn(a, _))
    }.value

  println(foldRight((1 to 100000).toList, 0L)(_ + _))



}
