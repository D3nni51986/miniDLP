package route

import actors.{ScanService}
import akka.actor.ActorSystem
import models._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{RequestContext, Route, RouteResult}
import akka.stream.ActorMaterializer

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
            system.actorOf(ScanService(ec, system, materializer)) ! HandleScanRequest(ctx)
          }
        }
      }
    }~pathPrefix("scan-file") {
      pathEndOrSingleSlash {
        post {
          imperativelyComplete { ctx =>
            system.actorOf(ScanService(ec, system, materializer)) ! HandleScanFileRequest(ctx)
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
