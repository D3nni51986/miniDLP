package actors

import akka.actor.Actor
import models._
import scan.ScanOperations.ScannerOps

abstract class ScannerWorker() extends Actor with ScannerOps{

  def scan(input:String):List[String] = {
    validations.map(_(input)).collect{
      case s:ScanResult if s.target.isDefined => s"Sensitive ${s.toString}  : ${s.target.getOrElse("")} has been detected"
    }
  }

  protected def returnScanResult(ctx:ScanServiceInputContext, validationResult: List[String]):ScanServiceResult
}

