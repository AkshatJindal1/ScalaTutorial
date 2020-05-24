package freemonad

import java.util.UUID

import cats.data.EitherK
import cats.free.Free
import cats.free.Free._
import cats.{Id, Inject, InjectK, ~>}


object Example5 extends App {

  type Symbol = String
  type Response = String

  sealed trait Orders[A]
  case class Buy(stock: Symbol, amount: Int) extends Orders[Response]
  case class Sell(stock: Symbol, amount: Int) extends Orders[Response]

  type OrdersF[A] = Free[Orders, A]

  def buy(stock: Symbol, amount: Int): OrdersF[Response] = liftF[Orders, Response](Buy(stock, amount))
  def sell(stock: Symbol, amount: Int): OrdersF[Response] = liftF[Orders, Response](Sell(stock, amount))

  def orderInterpreter: Orders ~> Id =
    new (Orders ~> Id) {
      override def apply[A](fa: Orders[A]): Id[A] =
        fa match {
          case Buy(stock, amount) =>
            println(s"Buying  $amount units of $stock")
            "ok"
          case Sell(stock, amount) =>
            println(s"Selling $amount units of $stock")
            "ok"
          case ListStock() =>
            println("This is implemented later in the code")
            Nil
        }
    }

  val smartTrade: OrdersF[Response] = for {
    _ <- buy("APPL", 100)
    _ <- buy("MSFT", 50)
    resp <- sell("GOOG", 150)
  } yield resp

  smartTrade.foldMap(orderInterpreter)

  // Adding error check
  import cats.syntax.either._
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]

  def eitherInterpreter: Orders ~> ErrorOr =
    new (Orders ~> ErrorOr) {
      override def apply[A](fa: Orders[A]): ErrorOr[A] =
        fa match {
          case Buy(stock, amount) =>
            s"$stock $amount".asRight
          case Sell(stock, amount) =>
            "Why are you selling that?".asLeft
          case ListStock() =>
            Nil.asRight

        }
    }
  println(s"\nError handelling with fail fast: ${smartTrade.foldMap(eitherInterpreter)}")

  // Extending our language to support whatstocks to buy without hard coding

  case class ListStock() extends Orders[List[Symbol]]

  def listStock(): OrdersF[List[Symbol]] =
    liftF[Orders, List[Symbol]](ListStock())

  import cats.implicits._

  val smartTradeWithList: OrdersF[Response] = for {
    st <- listStock()
    _ <- st.traverse(buy(_, 100))
    res <- sell("GOOG", 100)
  } yield res

  def orderPrinter: Orders ~> Id =
    new (Orders ~> Id) {
      override def apply[A](fa: Orders[A]): Id[A] =
        fa match {
          case ListStock() =>
            println("Getting list of stock: FB, TWTR")
            List("FB", "TWTR")
          case Buy(stock, amount) =>
            println(s"Buying  $amount units of $stock")
            "ok"
          case Sell(stock, amount) =>
            println(s"Selling $amount units of $stock")
            "ok"
        }
    }
  println("\nList implementation for stocks: ")
  smartTradeWithList.foldMap(orderPrinter)

  // LOGGING THE STEPS

  sealed trait Log[A]
  case class Info(msg: String) extends Log[Unit]
  case class Error(msg: String) extends Log[Unit]

  type LogF[A] = Free[Log, A]

  def info(msg: String): LogF[Unit] = liftF[Log, Unit](Info(msg))
  def error(msg: String): LogF[Unit] = liftF[Log, Unit](Error(msg))

  def logPrinter: Log ~> Id =
    new (Log ~> Id) {
      override def apply[A](fa: Log[A]): Id[A] =
        fa match {
          case Info(msg) =>
            println(s"[INFO]  - $msg")
          case Error(msg) =>
            println(s"[ERROR] - $msg")
      }
    }

  class OrderI[F[_]](implicit I: InjectK[Orders, F]) {
    def buyI(stock: Symbol, amount: Int): Free[F, Response] =
      Free.inject[Orders, F](Buy(stock, amount))
    def sellI(stock: Symbol, amount: Int): Free[F, Response] =
      Free.inject[Orders, F](Sell(stock, amount))
  }
  implicit def orderI[F[_]](implicit I: InjectK[Orders, F]): OrderI[F] = new OrderI[F]

  class LogI[F[_]](implicit I: InjectK[Log, F]) {
    def infoI(msg: String): Free[F, Unit] =
      Free.inject[Log, F](Info(msg))
    def errorI(msg: String): Free[F, Unit] =
      Free.inject[Log, F](Error(msg))
  }
  implicit def logI[F[_]](implicit I: InjectK[Log, F]): LogI[F] = new LogI[F]

  type TradeApp[A] = EitherK[Orders, Log, A]

  def smartTradeWithLogs(implicit O: OrderI[TradeApp],
                         L: LogI[TradeApp]): Free[TradeApp, Response] = {
    import O._
    import L._

    for {
      _ <- infoI("I am going to trade.")
      _ <- buyI("APPL", 100)
      _ <- infoI(" I will trade smartly now")
      _ <- buyI("MSFT", 100)
      rsp <- sellI("GOOG", 100)
      _ <- errorI("Not possible")
    } yield rsp
  }

  def composedInterpreter: TradeApp ~> Id = orderPrinter or logPrinter
  println("\nImplementation for logs: ")
  smartTradeWithLogs.foldMap(composedInterpreter)

  //  ADDING A THIRD FEATURE
  type UserId = String
  type JobId = String
  type Values = String

  sealed trait Audit[A]
  case class UserActionAudit(user: UserId, action: String, values: List[Values]) extends Audit[Unit]
  case class SystemActionAudit(job: JobId, action: String, values: List[Values]) extends Audit[Unit]

  class AuditI[F[_]](implicit I: InjectK[Audit, F]){
    def userAction(user: UserId, action: String, values: List[Values]): Free[F, Unit] =
      Free.inject[Audit, F](UserActionAudit(user, action, values))
    def systemAction(job: JobId, action: String, values: List[Values]): Free[F, Unit] =
      Free.inject[Audit, F](SystemActionAudit(job, action, values))
  }

  implicit def auditI[F[_]](implicit I: InjectK[Audit, F]): AuditI[F] = new AuditI[F]

  def auditPrinter: Audit ~> Id =
    new(Audit ~> Id) {
      override def apply[A](fa: Audit[A]): Id[A] =
        fa match {
          case UserActionAudit(user, action, values) =>
            println(s"[USER Action]   - user $user called $action with values $values")
          case SystemActionAudit(job, action, values) =>
            println(s"[SYSTEM Action] - job $job called $action with values $values")
        }
    }


  type AuditableTradeApp[A] = EitherK[Audit, TradeApp, A]

  def auditableInterpreter: AuditableTradeApp ~> Id = auditPrinter or composedInterpreter

  def smartTradeWithAuditAndLogs(implicit O: OrderI[AuditableTradeApp],
                                 L: LogI[AuditableTradeApp],
                                 A: AuditI[AuditableTradeApp]): Free[AuditableTradeApp, Response] = {
    import O._, L._, A._

    for {
      _ <- infoI("I'm going to trade smartly")
      _ <- userAction("ID102", "buy", List("APPL", "100"))
      _ <- buyI("APPL", 200)
      _ <- infoI("I'm going to trade even more smartly")
      _ <- userAction("ID102", "buy", List("MSFT", "100"))
      _ <- buyI("MSFT", 100)
      _ <- userAction("ID102", "sell", List("GOOG", "100"))
      rsp <- sellI("GOOG", 300)
      _ <- systemAction("BACKOFFICE", "tradesCheck", List("ID102", "lastTrades"))
      _ <- errorI("Wait, what?!")
    } yield rsp
  }
  println("\nImplementation for audits: ")
  smartTradeWithAuditAndLogs.foldMap(auditableInterpreter)

  // IMPLEMENTING A MESSAGING INTERFACE

  type ChannelId = String
  type SourceId = String
  type MessageId = String
  type Payload  = String
  type Condition = String

  sealed trait Messaging[A]
  case class Publish(channelId: ChannelId, source: SourceId, messageId: MessageId, payload: Payload) extends Messaging[Payload]
  case class Subscribe(channelId: ChannelId, condition: Condition) extends Messaging[Payload]

  type MessagingF[A] = Free[Messaging, A]

  def publish(channelId: ChannelId, source: SourceId, messageId: MessageId, payload: Payload): MessagingF[Response] =
    liftF[Messaging, Response](Publish(channelId, source, messageId, payload))
  def subscribe(channelId: ChannelId, condition: Condition): MessagingF[Payload] =
    liftF[Messaging, Payload](Subscribe(channelId, condition))

  def messagingPrinter: Messaging ~> Id =
    new (Messaging ~> Id) {
      override def apply[A](fa: Messaging[A]): Id[A] =
        fa match {
          case Publish(channelId, source, messageId, payload) =>
            println(s"Publish [$channelId] From: [$source] Id: [$messageId] Payload: [$payload]")
            "ok"
          case Subscribe(channelId, condition) =>
            val payload = "Event fired"
            println(s"Received message from [$channelId](filter: [$condition]): [$payload]")
            payload
        }
    }

  def orderToMessageInterpreter: Orders ~> MessagingF =
    new (Orders ~> MessagingF) {
      override def apply[A](fa: Orders[A]): MessagingF[A] =
        fa match {
          case ListStock() =>
            for {
              _ <- publish("001", "Orders", UUID.randomUUID().toString, "Get Stocks List")
              paylod <- subscribe("001", "*")
            } yield List(paylod)

          case Buy(stock, amount) =>
            publish("001", "Orders", UUID.randomUUID().toString, s"Buy $stock $amount")

          case Sell(stock, amount) =>
            publish("001", "Orders", UUID.randomUUID().toString, s"Sell $stock $amount")

        }
    }

  def messagingFreePrinter: MessagingF ~> Id =
    new(MessagingF ~> Id) {
      override def apply[A](fa: MessagingF[A]): Id[A] =
        fa.foldMap(messagingPrinter)
    }

  def ordersToTerminalViaMessage: Orders ~> Id =
    orderToMessageInterpreter andThen messagingFreePrinter

  println("\nTrade via messaging service")
  smartTradeWithList.foldMap(ordersToTerminalViaMessage)

  def composedViaMessageInterpreter: TradeApp ~> Id =
    ordersToTerminalViaMessage or logPrinter

  def auditableToTerminalViaMessage: AuditableTradeApp ~> Id =
    auditPrinter or composedViaMessageInterpreter
  println("\nTrade via message with all features")
  smartTradeWithAuditAndLogs.foldMap(auditableToTerminalViaMessage)









}
