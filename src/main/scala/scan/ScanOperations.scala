package scan

import models.{CreditCardNumberScanResult, ScanResult, SocialSecurityNumberScanResult}

import scala.util.matching.Regex

object ScanOperations {

  import SecurityConsts._

  trait ScannerOps {
    val validations:List[String => ScanResult]
    def scan(input:String):List[String]
  }

  def checkCreditCard(input:String):CreditCardNumberScanResult = {
    regexMach(SecurityConsts.creditCardRegexList, input) match {
      case Some(creditCardNumber)
        if contextKeyordsValidation(SecurityConsts.CreditCardNumberContextsKeywords, input) => CreditCardNumberScanResult(Some(creditCardNumber))
      case _                                                                                => CreditCardNumberScanResult(None)
    }
  }

  def checkSocialSecurity(input:String):SocialSecurityNumberScanResult = {
    regexMach(socialSecurityRegexList, input) match {
      case Some(socialSecurityNumber)
        if contextKeyordsValidation(SecurityConsts.SocialSecurityContextKeywords, input) => SocialSecurityNumberScanResult(Some(socialSecurityNumber))
      case _                                                                             => SocialSecurityNumberScanResult(None)
    }
  }

  def contextKeyordsValidation(keywords:Set[String], input:String) = {
    keywords.exists(input.contains(_))
  }

  def regexMach(regex:Regex, input:String) = {
    regex.findFirstIn(input)
  }

  def luhnValidation(input:String) = {

  }
}
