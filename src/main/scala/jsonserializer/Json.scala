package jsonserializer

import jsonserializer.JsonSerializer.{Json, JsonWriter}

//  Interface object to create interface
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}
