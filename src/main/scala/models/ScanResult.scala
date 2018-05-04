package models

abstract class ScanResult() {
  def target:Option[String]
  override def toString: String = {
    val regex = "(?<=.)(?=\\p{Lu})"
    this.getClass.getSimpleName.replaceFirst("ScanResult", "").split(regex).foldLeft(""){(a,b) => a + " " + b}
  }
}

case class SocialSecurityNumberScanResult(target:Option[String]) extends ScanResult
case class CreditCardNumberScanResult(target:Option[String]) extends ScanResult

