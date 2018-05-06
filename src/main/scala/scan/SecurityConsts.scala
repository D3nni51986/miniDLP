package scan

object SecurityConsts {

  val SocialSecurityContextKeywords = Set[String]("Social", "Security", "Social Security#",
    "Soc Sec", "SSN", "SSNS", "SSN#", "SS#", "SSID")
    .map(_.toLowerCase)

  val CreditCardNumberContextsKeywords = Set[String]("amex", "american express", "americanexpress", "Visa, mastercard", "master card", "mc", "mastercards", "master cards", "diner's Club", "diners club",
    "dinersclub", "discover card", "discovercard", "discover cards", "carte blanche",
    "carteblanche", "credit card", "cc#", "bank card", "bankcard", "card number", "card num", "cardnumber", "cardnumbers", "card numbers", "creditcard", "credit cards",
    "creditcards", "ccn", "debit card", "debitcard", "debit cards", "debitcards", "atm card", "atmcard", "atm cards", "atmcards", "carte bancaire", "carte de crédit",
    "carte de credit", "numéro de carte", "numero de carte", "no de la carte", "no de carte", "kreditkarte", "karte", "karteninhaber", "karteninhabers",
    "kreditkarteninhaber", "kreditkarteninstitut", "kreditkartentyp", "kartennr",
    "kartennummer", "kreditkartennummer", "kreditkarten-nummer", "carta di credito", "carta credito", "numero carta", "numero della carta", "numero di carta",
    "cartão de crédito", "cartão de credito", "cartao de crédito", "cartao de credito", "cartão de débito", "cartao de débito", "cartão de debito", "cartao de debito",
    "débito automático", "debito automatico", "número do cartão",
    "numero do cartão", "número do cartao", "numero do cartao", "número de cartão", "numero de cartão", "número de cartao", "numero de cartao")
    .map(_.toLowerCase)

  val socialSecurityRegexList = List("""(\d\d\d\d\d\d\d\d\d)""",
    """(\d\d\d)-(\d\d)-(\d\d\d\d)""",
    """(\d\d\d) (\d\d) (\d\d\d\d)""")
    .foldLeft("")((a, b) => a + "|"  + b).drop(1).r

  val creditCardRegexList = List("""(\d){13,16}""",
    """(\d\d\d\d)-(\d\d\d\d)-(\d\d\d\d)-(\d\d\d\d)""",
    """(\d\d\d\d) (\d\d\d\d) (\d\d\d\d) (\d\d\d\d)""")
    .foldLeft("")((a, b) => a + "|"  + b).drop(1).r

  val luhnMap = Map[Int,Int](1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 1, 6 -> 3, 7 -> 5, 8 -> 7, 9 -> 9, 0 -> 0)

  val creditCardCleanRegex = "-| ".r
}
