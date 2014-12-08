package web

import akka.io.IO
import api.Api
import spray.can.Http

//trait RestWebServer extends CoreActors with Core with Api {
//  this: Api with CoreActors with Core =>
trait RestWebServer {
  this: Api =>

  IO(Http)(system) ! Http.Bind(rootService, "0.0.0.0", port = 8080)

}
