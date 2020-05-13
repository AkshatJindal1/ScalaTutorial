package jsonserializer

import jsonserializer.JsonSerializer.{Json, JsonWriter}

object JsonSyntax {

  //  Extension methods/interface syntax to extend existing types
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

}
