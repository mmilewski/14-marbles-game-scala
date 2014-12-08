package api

import akka.actor.ActorRef
import spray.routing.Directives

import scala.concurrent.ExecutionContext

class MessagingService(val messaging: ActorRef)
                      (implicit exeContext: ExecutionContext)
  extends Directives {

  val routes =
    path("msg") {
      get {
        complete {
          "Received something"
        }
      }
    }
}
