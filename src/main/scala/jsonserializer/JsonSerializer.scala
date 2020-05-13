package jsonserializer



object JsonSerializer {


  //  Define a json abstract simple tree
  sealed trait Json

  /*
      serialize to json is encoded in this trait
      JsonWriter is the type class here
   */
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsNumber(get: Double) extends Json

  final case class Person(name: String, email: String)

  case object JsNull extends Json




}
