import actors.Scanner
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import route.ServiceRoute

import scala.io.StdIn

object Service extends App with ServiceRoute{

  implicit val system = ActorSystem()
  implicit val ec = system.dispatcher
  implicit val materializer = ActorMaterializer()

  Http().bindAndHandle(route, "localhost", 8080)

  println("ENTER to terminate")
  StdIn.readLine()
  system.terminate()
}
