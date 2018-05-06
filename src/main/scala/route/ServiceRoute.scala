package route

import actors.Scanner
import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import models._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, RequestContext, Route, RouteResult}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}

import scala.concurrent.{ExecutionContext, Promise}

trait ServiceRoute {

  implicit val ec:ExecutionContext
  implicit val system:ActorSystem
  implicit val materializer:ActorMaterializer

  val route =
    pathPrefix("scan") {
      pathEndOrSingleSlash {
        post {
          imperativelyComplete { ctx =>
            system.actorOf(Scanner(ec, system, materializer)) ! HandleScanRequest(ctx)
          }
        }
      }
    }~pathPrefix("scan-file") {
      pathEndOrSingleSlash {
        post {
          imperativelyComplete { ctx =>
            system.actorOf(Scanner(ec, system, materializer)) ! HandleScanFileRequest(ctx)
          }
        }
      }
    }

  def imperativelyComplete(inner: ImperativeRequestContext => Unit): Route = { ctx: RequestContext =>
    val p = Promise[RouteResult]()
    inner(new ImperativeRequestContext(ctx, p))
    p.future
  }
}
