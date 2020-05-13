package catscustominstances

import java.util.Date

import cats._
import cats.implicits._
import datatype.Cat

object CatShowInstances {

  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime}ms since the epoch.")

  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name.show} is a " +
      s"${cat.age.show} year-old " +
      s"${cat.color.show} cat.")

}
