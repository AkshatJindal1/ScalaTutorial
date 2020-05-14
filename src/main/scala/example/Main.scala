package example

import java.util.Date

import cats.{Contravariant, Show, Monoid}
import datatype.{Box, Branch, Cat, Leaf, Order, Tree}
import jsonserializer.JsonSyntax._
import jsonserializer.JsonWriterInstances._
import jsonserializer.Json
import jsonserializer.JsonSerializer.Person
import printablelibrary.Printable
import printablelibrary.PrintableInstances._
import printablelibrary.PrintableSyntax._
import custominstances.CatShowInstances._
import custominstances.CatsEqInstances._
import custominstances.OrderMonoidInstances._
import cats.syntax.show._
import cats.syntax.eq._
import cats.instances.option._
import cats.instances.int._
import cats.instances.string._
import monoidsandsemigroups.MonoidInstances._
import monoidsandsemigroups.MonoidSyntax._
import custominstances.TreeFunctorInstances._
import cats.syntax.functor._
import cats.syntax.contravariant._
import cats.syntax.semigroup._
import custominstances.SymbolMonoidInstance._


object Main extends App {

  println("Hello, World from Akshat. This is my first Scala program")
  println("My age is 24")


  println(Json.toJson(Person("Akshat", "ak@jin.con")))

  println(Person("Akshat Jindal", "aj@aj.com").toJson)
  println("Akshat".toJson)
  println((1.0).toJson)
  println(Option("A string").toJson)
  println(Option(1.0).toJson)

  //  Printable class type
  Printable.print("Akshat")
  Printable.print(1)

  val cat = Cat("Kitty", 2, "brown")
  Printable.print(cat)

  cat.print
  true.print

  Box("12 in box").print
  Option("12").print

  val cat2 = Cat("Billu", 4, "white")
  println(cat2.show)
  val now = new Date()
  val fewMomentsLater = new Date()
  println(s"${now.getTime} ${fewMomentsLater.getTime}")
  println(now === fewMomentsLater)

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat3 = Cat("Heathcliff", 33, "orange and black")

  println(cat1 === cat3)
  println((cat1 =!= cat3))

  println(Option(cat1) === Option.empty[Cat])
  println(Option(cat2) =!= Option.empty[Cat])

  //Monoids
//  combine(true, false)
//  true combine false  -> wont work as we have two implementation of monoids of booleanAn
  println(Set(1,2,3) combine Set(2,3,4))

  //  Super Adder
  val nums: List[Int] = List(1,2,3,4)
  //  1. Without monoid
  println(SuperAdder.superAdder(nums))
  //  2. With monoids
  println(SuperAdder.superAdderMonoid(nums))
  println(SuperAdder.superAdderMonoid(
    List( Option(1), Option(2), Option(3))))

  //  Super adder for orders
  val orderList = List(Order(12, 6), Order(14,10), Order(124, 32))
  println(s"Total order and sum is ${SuperAdder.superAdderMonoid(orderList)}")

  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))
  println(Tree.leaf(100).map(_ * 2))

  //  Contravarient in Cats
  val showString = Show[String]

  val showSymbol = Contravariant[Show].
    contramap(showString)((symbol: Symbol) => s"${symbol.name}")

  println(showSymbol.show(Symbol("abc")).getClass)

  println(showString.contramap[Symbol](_.name).show(Symbol("abc")))

  println(Symbol("abcd").name.getClass)

  // Invarient in cats
  println(Monoid[Symbol].empty)
  println(Symbol("a") |+| Symbol("ksha") |+| Symbol("t"))












}
