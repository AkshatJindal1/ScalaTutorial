package example

import java.util.Date

import datatype.Cat
import jsonserializer.JsonSyntax._
import jsonserializer.JsonWriterInstances._
import jsonserializer.Json
import jsonserializer.JsonSerializer.Person
import printablelibrary.Printable
import printablelibrary.PrintableInstances._
import printablelibrary.PrintableSyntax._
import catscustominstances.CatShowInstances._
import catscustominstances.CatsEqInstances._
import cats.syntax.show._
import cats.syntax.eq._
import cats.instances.option._


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










}
