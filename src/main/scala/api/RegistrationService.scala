package api

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import spray.routing.Directives

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

class RegistrationService(val registration: ActorRef)
                         (implicit exeContext: ExecutionContext)
  extends Directives with DefaultJsonProtocol with SprayJsonSupport {

  implicit val timeout = Timeout(2.seconds)

  import core.RegistrationActor._

  implicit val registerFormat = jsonFormat1(Register)
  implicit val registeredFormat = jsonFormat2(Registered)
  implicit val notRegisteredFormat = jsonFormat2(NotRegistered)

  val routes =
    pathPrefix("register") {
      pathEnd {
        get {
          complete {
            Registered("user-" + Math.abs(Random.nextLong()))
          }
        }
      } ~
        path("new") {
          post {
            handleWith {
              reg: Register =>
                (registration ? reg).mapTo[Either[NotRegistered, Registered]]
            }
          }
        }
    }
}
