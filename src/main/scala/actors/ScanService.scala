package actors

import akka.actor.{ActorSystem, Props}
import akka.pattern.pipe
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import models.{ScanServiceInputContext, _}
import route.ImperativeRequestContext
import scan.ScanOperations
import support.JsonSupport

import scala.concurrent.{ExecutionContext}
import scala.io.Source

object ScanService{
  def apply(implicit ec: ExecutionContext, system: ActorSystem, materializer: ActorMaterializer):Props = Props(new ScanService())
}

class ScanService(implicit ec: ExecutionContext, system: ActorSystem, materializer: ActorMaterializer)
  extends ScannerWorker
    with JsonSupport{

  val validations = List[String => ScanResult](
    ScanOperations.checkCreditCard,
    ScanOperations.checkSocialSecurity)

  override def receive: Receive = {
    case HandleScanRequest(ctx)     => extractScanRequest(ctx)
    case HandleScanFileRequest(ctx) => extractScanFileRequest(ctx)
    case s:ScanServiceInputContext  => completeSession(s, scanResult(s))
    case _                          => ScanServiceResult("" , List("Incorrect service call"))
  }

  def scanResult(inputCtx:ScanServiceInputContext) = {
    require(inputCtx.input.input.length <= 4000)
    returnScanResult(inputCtx, scan(inputCtx.input.input.toLowerCase()))
  }

  def completeSession(inputCtx:ScanServiceInputContext, scanServiceResult:ScanServiceResult) = {
    val scanServiceResult = scan(inputCtx.input.input.toLowerCase())
    inputCtx.ctx.complete(returnScanResult(inputCtx, scanServiceResult))
    context.stop(self)
  }

  def extractScanRequest(ctx:ImperativeRequestContext) = {
    val f = Unmarshal(ctx.request.entity).to[ScanServiceInput].map{
      case s:ScanServiceInput => ScanServiceInputContext(s, ctx)
    }
    pipe(f) to self
  }

  def extractScanFileRequest(ctx:ImperativeRequestContext) = {
    val f = Unmarshal(ctx.request.entity).to[ScanFileRequest].map{
      case sf:ScanFileRequest  =>
        val input = Source.fromFile(sf.filePath).getLines.mkString
        ScanServiceInputContext(ScanServiceInput(sf.id, input), ctx)
    }
    pipe(f) to self
  }

  override def returnScanResult(ctx:ScanServiceInputContext, validationResult: List[String]) = {
    validationResult.nonEmpty match {
      case true  => ScanServiceResult(ctx.input.id, validationResult)
      case false => ScanServiceResult(ctx.input.id, List("Clean input"))
    }
  }
}



