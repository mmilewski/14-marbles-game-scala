package core

import akka.actor.ActorSystem

trait BootedCore extends Core {
  val system = ActorSystem("engine")

  sys.addShutdownHook(system.shutdown())
}
