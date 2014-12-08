package api

import akka.actor.Props
import core.{Core, CoreActors}
import spray.routing.Directives

import scala.concurrent.ExecutionContext

trait Api extends CoreActors with Core with Directives {
  private implicit val _: ExecutionContext = system.dispatcher

  val routes =
    new RegistrationService(registration).routes ~
      new MessagingService(messenger).routes

  val rootService = system.actorOf(Props(new RoutedHttpService(routes)))

}
