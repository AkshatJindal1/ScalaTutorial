package casestudy.crdt

import cats.kernel.CommutativeMonoid
import cats.instances.list._
import cats.syntax.semigroup._
import cats.syntax.foldable._
import KeyValueStoreSyntax._

trait GCounter[F[_,_],K, V] {

  def increment(f: F[K,V])(k: K, v: V)
               (implicit m: CommutativeMonoid[V]): F[K,V]

  def merge(f1: F[K,V], f2: F[K,V])
           (implicit m: BoundedSemiLattice[V]): F[K,V]

  def total(f: F[K,V])
           (implicit m: CommutativeMonoid[V]): V

}

object GCounter {
  def apply[F[_,_], K, V](implicit counter: GCounter[F,K,V]) =
    counter

  implicit def gCounterInstance[F[_,_],K,V]
      (implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K,V]]): GCounter[F,K,V] =
    new GCounter[F,K,V] {

      override def increment(f: F[K, V])(k: K, v: V)
                            (implicit m: CommutativeMonoid[V]): F[K, V] = {
        val value = v |+| f.getOrElse(k,m.empty)
        f.put(k, value)
      }
      
      override def merge(f1: F[K, V], f2: F[K, V])
                        (implicit m: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

      override def total(f: F[K, V])
                        (implicit m: CommutativeMonoid[V]): V = f.values.combineAll
    }

}
