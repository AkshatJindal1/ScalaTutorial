package casestudy.testingasynchronouscode

import cats.Applicative
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._ // for map

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class UptimeService[F[_]](client: UptimeClient[F])(implicit a: Applicative[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)

}
