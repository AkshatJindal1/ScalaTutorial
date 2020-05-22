package casestudy.testingasynchronouscode

object UnitTest extends App {

  def testUptimeClient() = {
    val host = Map("host1" -> 10, "host2" -> 7)
    val client = new TestUptimeClient(host)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(host.keys.toList)
    val expected = host.values.sum
    assert(actual == expected)
  }

  testUptimeClient

}
