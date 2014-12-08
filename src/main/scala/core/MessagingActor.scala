package core

import akka.actor.Actor

class MessagingActor extends Actor {
  override def receive: Receive = {
    case _ =>
      sender ! "hello stranger"
  }
}
