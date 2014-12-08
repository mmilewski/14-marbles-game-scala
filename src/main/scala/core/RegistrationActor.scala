package core

import akka.actor.{Actor, ActorLogging}

object RegistrationActor {

  case class Register(firstName: String)

  case class NotRegistered(reason: String, event: String = "notRegistered")

  case class Registered(userId: String, event: String = "registered")

}

class RegistrationActor extends Actor with ActorLogging {

  import core.RegistrationActor._

  override def receive: Receive = {
    case Register(username) =>
      val userId = java.util.UUID.randomUUID().toString
      log.info(s"Registering user: `$username` with uuid: `$userId`")
      if (username.startsWith("m")) {
        sender ! Left(NotRegistered("Name is already taken"))
      }
      else {
        sender ! Right(Registered(userId))
      }
  }
}
