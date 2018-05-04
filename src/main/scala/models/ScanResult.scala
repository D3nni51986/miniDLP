package models

abstract class ScanResult(target:Option[String])
case class SocialSecurityNumberScanResult(target:Option[String]) extends ScanResult(target)
case class CreditCardNumberScanResult(target:Option[String]) extends ScanResult(target)

