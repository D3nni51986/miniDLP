import akka.actor.Actor

import scala.util.Random


sealed trait SensetiveDataType
case object Iban extends SensetiveDataType
case object SocialSecurityNumber extends SensetiveDataType
case object CreditCardNumber extends SensetiveDataType

object ContextWords{
  val SocialSecurityContextKeywords = Set[String]("Social", "Security", "Social Security#",
    "Soc Sec", "SSN", "SSNS", "SSN#", "SS#", "SSID").map(_.toLowerCase)

  val CreditCardNumberContextsKeywords = Set[String]("amex", "american express", "americanexpress", "Visa, mastercard", "master card", "mc", "mastercards", "master cards", "diner's Club", "diners club",
  "dinersclub", "discover card", "discovercard", "discover cards", "carte blanche",
  "carteblanche", "credit card", "cc#", "bank card", "bankcard", "card number", "card num", "cardnumber", "cardnumbers", "card numbers", "creditcard", "credit cards",
  "creditcards", "ccn", "debit card", "debitcard", "debit cards", "debitcards", "atm card", "atmcard", "atm cards", "atmcards", "carte bancaire", "carte de crédit",
  "carte de credit", "numéro de carte", "numero de carte", "no de la carte", "no de carte", "kreditkarte", "karte", "karteninhaber", "karteninhabers",
  "kreditkarteninhaber", "kreditkarteninstitut", "kreditkartentyp", "kartennr",
  "kartennummer", "kreditkartennummer", "kreditkarten-nummer", "carta di credito", "carta credito", "numero carta", "numero della carta", "numero di carta",
    "cartão de crédito", "cartão de credito", "cartao de crédito", "cartao de credito", "cartão de débito", "cartao de débito", "cartão de debito", "cartao de debito",
    "débito automático", "debito automatico", "número do cartão",
    "numero do cartão", "número do cartao", "numero do cartao", "número de cartão", "numero de cartão", "número de cartao", "numero de cartao").map(_.toLowerCase)
}

object Regex{

  val socialSecurityRegexList =
    List("""(\d\d\d\d\d\d\d\d\d)""",
      """(\d\d\d)-(\d\d)-(\d\d\d\d)""",
      """(\d\d\d) (\d\d) (\d\d\d\d)""")
      .foldLeft("")((a, b) => a + "|"  + b).drop(1).r

  val creditCardRegexList =
    List("""(\d){13,16}""",
      """(\d\d\d\d)-(\d\d\d\d)-(\d\d\d\d)-(\d\d\d\d)""",
      """(\d\d\d\d) (\d\d\d\d) (\d\d\d\d) (\d\d\d\d)""")
      .foldLeft("")((a, b) => a + "|"  + b).drop(1).r

}

object Service extends App{

  val list = List[String => String](checkSocialSecurity, checkCreditCard)

  def checkCreditCard(input:String):String = {
    val inputLowerCase = input.toLowerCase
    var scanResult = ""
    Regex.creditCardRegexList.findFirstIn(inputLowerCase) match {
      case Some(creditCardNumber) => ContextWords.CreditCardNumberContextsKeywords.exists({
        contextKeyWord =>
          val res = inputLowerCase.contains(contextKeyWord)
          if (res){
            scanResult = creditCardNumber + " " + contextKeyWord
          }
          res
      })
      case None                   => scanResult = "clean"
    }

    CreditCardNumber + ": " + scanResult
  }

  def checkSocialSecurity(input:String):String = {
    val inputLowerCase = input.toLowerCase
    var scanResult = ""
    Regex.socialSecurityRegexList.findFirstIn(inputLowerCase) match {
      case Some(socialSecurityNumber) => ContextWords.SocialSecurityContextKeywords.exists({
        contextKeyWord =>
          val res = inputLowerCase.contains(contextKeyWord)
          if (res){
            scanResult = socialSecurityNumber + " " + contextKeyWord
          }
          res
      })
      case None                       => scanResult = "clean"
    }

    SocialSecurityNumber + ": " + scanResult
  }

  def test() = {
    list.foreach(scan => println(scan("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\n")))
  }

  test

  def randomAlphaNumericString(length: Int): String = {
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')
    randomStringFromCharList(length, chars)
  }

  def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
    val sb = new StringBuilder
    for (i <- 1 to length) {
      val randomNum = util.Random.nextInt(chars.length)
      sb.append(chars(randomNum))
    }
    sb.toString
  }

}
