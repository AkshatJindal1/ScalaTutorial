package casestudy.testingasynchronouscode

import cats.Id

import scala.concurrent.Future

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {

  override def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}
