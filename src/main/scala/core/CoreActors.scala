package core

import akka.actor.Props

trait CoreActors {
  this: Core =>

  val registration = system.actorOf(Props[RegistrationActor], "registration")
  val messenger = system.actorOf(Props[MessagingActor], "messenger")

}

