package casestudy.testingasynchronouscode

import cats.Id

trait TestUptime extends UptimeClient[Id] {

  def getUptime(hostname: String): Id[Int]

}
