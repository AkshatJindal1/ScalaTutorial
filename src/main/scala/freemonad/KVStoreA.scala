package freemonad

sealed trait KVStoreA[A]

case class Put[T](key: String, value: T) extends KVStoreA[Unit]

case class Get[T](key: String) extends KVStoreA[Option[T]]

case class Delete(key: String) extends KVStoreA[Unit]