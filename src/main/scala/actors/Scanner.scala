package actors

import akka.actor.{Actor, Props}
import models._
import scan.ScanOperations

object Scanner{
  def apply():Props = Props(new Scanner())
}

class Scanner() extends ScannerWorker {

  val validations = List[String => ScanResult](
    ScanOperations.checkCreditCard,
    ScanOperations.checkSocialSecurity)

  override def receive: Receive = {
    case ScanServiceInput(input) => scanInput(input.toLowerCase())
    case _                       => ScanServiceResult(List("Incorrect service call"))
  }
}

trait ScannerOps {
  val validations:List[String => ScanResult]
}

abstract class ScannerWorker() extends Actor with ScannerOps{
  protected def scanInput(input:String) = {
    val scanResultList = validations
      .map(_(input.toLowerCase))
      .collect{
        case s:SocialSecurityNumberScanResult => s"Sensetive Social Security Number => ${s.target.getOrElse("")} has been detected"
        case c:CreditCardNumberScanResult     => s"Sensetive Credit Card Number => ${c.target.getOrElse("")} has been detected"
      }

    scanResultList.nonEmpty match {
      case true  => ScanServiceResult(scanResultList)
      case false => ScanServiceResult(List("Clean input"))
    }
  }
}



