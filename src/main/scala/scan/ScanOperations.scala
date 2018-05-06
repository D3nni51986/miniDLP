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
      case creditCardNumbers:List[String]  =>
        val findCreditCard = checkAllCreditCards(creditCardNumbers, input)
        CreditCardNumberScanResult(findCreditCard)
      case _                               => CreditCardNumberScanResult(None)
    }
  }

  def checkSocialSecurity(input:String):SocialSecurityNumberScanResult = {
    regexMach(socialSecurityRegexList, input) match {
      case socialSecurityNumbers:List[String] =>
        val findSocialNumber = checkAllSocialSecurityNumbers(socialSecurityNumbers, input)
        SocialSecurityNumberScanResult(findSocialNumber)
      case _                                  => SocialSecurityNumberScanResult(None)
    }
  }

  private def checkAllCreditCards(creditCardNumbers:List[String], input:String) = {
    creditCardNumbers.find(cc => contextKeyordsValidation(SecurityConsts.CreditCardNumberContextsKeywords, input)
      && luhnValidation(creditCardCleanRegex.replaceAllIn(cc, "").reverse))
  }

  private def checkAllSocialSecurityNumbers(socialSecurityNumbers:List[String], input:String):Option[String] = {
    socialSecurityNumbers.find(ss => contextKeyordsValidation(SecurityConsts.SocialSecurityContextKeywords, input))
  }

  private def contextKeyordsValidation(keywords:Set[String], input:String) = {
    keywords.exists(input.contains(_))
  }

  private def regexMach(regex:Regex, input:String) = {
    regex.findAllIn(input).toList
  }

  def luhnValidation(reversedInput:String) = {
    var toDouble = true
    reversedInput.foldLeft(0){ (a,b) =>
      toDouble = !toDouble; if (toDouble) { a + luhnMap( b - '0'.toInt)} else{a +  b - '0'.toInt}} % 10 == 0
  }
}
