package example

import jsonserializer.JsonSyntax._
import jsonserializer.JsonWriterInstances._
import jsonserializer.Json
import jsonserializer.JsonSerializer.Person
import printablelibrary.Printable
import printablelibrary.PrintableInstances._

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

}
