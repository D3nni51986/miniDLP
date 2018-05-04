package scan

import models.{CreditCardNumberScanResult, SocialSecurityNumberScanResult}

object ScanOperations {

  def checkCreditCard(input:String):CreditCardNumberScanResult = {
    SecurityConsts.creditCardRegexList.findFirstIn(input) match {
      case Some(creditCardNumber)
        if (SecurityConsts.CreditCardNumberContextsKeywords.exists(input.contains(_))) => CreditCardNumberScanResult(Some(creditCardNumber))
      case _                                                                           => CreditCardNumberScanResult(None)
    }
  }

  def checkSocialSecurity(input:String):SocialSecurityNumberScanResult = {
    SecurityConsts.socialSecurityRegexList.findFirstIn(input) match {
      case Some(socialSecurityNumber)
        if SecurityConsts.SocialSecurityContextKeywords.exists(input.contains(_)) => SocialSecurityNumberScanResult(Some(socialSecurityNumber))
      case _                                                                      => SocialSecurityNumberScanResult(None)
    }
  }
}
