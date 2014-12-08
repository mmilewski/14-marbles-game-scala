package web

import api.Api
import core.{BootedCore, CoreActors}

object RestWebServerBootstrap extends App with BootedCore with CoreActors with Api with RestWebServer {
  private val serverName = RestWebServerBootstrap.getClass.getName
  println(s"Starting $serverName class with parameters ${args.toList}")
}
