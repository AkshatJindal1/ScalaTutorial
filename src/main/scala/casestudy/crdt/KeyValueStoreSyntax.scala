package casestudy.crdt

object KeyValueStoreSyntax {

  implicit class KvsOps[F[_,_], K, V](f: F[K,V]) {

    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K,V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }

}
