package casestudy.testingasynchronouscode
import scala.concurrent.Future

// this trait is used in production. This will be asynchronous
trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}
