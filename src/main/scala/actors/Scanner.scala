package actors

import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.model.HttpEntity
import akka.pattern.pipe
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import models.{ScanServiceInputContext, _}
import scan.ScanOperations
import support.JsonSupport

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

object Scanner{
  def apply(implicit ec: ExecutionContext, system: ActorSystem, materializer: ActorMaterializer):Props = Props(new Scanner())
}

class Scanner(implicit ec: ExecutionContext, system: ActorSystem, materializer: ActorMaterializer)
  extends ScannerWorker
    with JsonSupport{

  val validations = List[String => ScanResult](
    ScanOperations.checkCreditCard,
    ScanOperations.checkSocialSecurity)

  override def receive: Receive = {
    case HandleScanRequest(ctx)     => extractScanRequest(ctx)
    case HandleScanFileRequest(ctx) => extractScanFileRequest(ctx)
    case s:ScanServiceInputContext  => completeSession(s)
    case _                          => ScanServiceResult("" , List("Incorrect service call"))
  }

  def completeSession(inputCtx:ScanServiceInputContext) = {
    try{
      val scanResult    = scan(inputCtx.input.input.toLowerCase())
      val comleteResult = returnScanResult(inputCtx, scanResult)
      inputCtx.ctx.complete(comleteResult)
      context.stop(self)
    }
    catch{
      case t:Throwable =>
        inputCtx.ctx.fail(t)
        context.stop(self)
    }
  }

  def extractScanRequest(ctx:ImperativeRequestContext) = {
    val f = Unmarshal(ctx.request.entity).to[ScanServiceInput].map{
      case s:ScanServiceInput => ScanServiceInputContext(s, ctx)
      case _                  => throw new Exception
    }
    pipe(f) to self
  }

  def extractScanFileRequest(ctx:ImperativeRequestContext) = {
    val f = Unmarshal(ctx.request.entity).to[ScanFileRequest].map{
      case s:ScanFileRequest  =>
        val input = Source.fromFile(s.filePath).getLines.mkString
        ScanServiceInputContext(ScanServiceInput(s.id, input), ctx)
      case _                  => throw new Exception
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



