package financial

import financial.Contract.Amount

/**
  *
  * siehe pj-eber.pdf
  *
  * Methode:
  * 1. einfaches Beispiel für Domänproject einholen
  * 2. die modellieren -> möglicherweise Sackgasse
  * 3. einfache Beispiele in atomare Bestandteile zerlegen
  * 4. nach Selbstreferenzen suchen
  * 5. ggf. mit weiteren Beispielen wiederholen
  * 6. nach binären Operator suchen
  * 7. wenn 6. erfolgreich: dann suche nach neutralem Element
  */

/**
  * Einfacher Vertag (Zero-Coupon Bond / Zero Bond)
  *
  * Bekomme 100 Pfund am 29.01.2001
  * Bekomme 200 Eur am 31.12.2021
  *
  * Bezahle 100 Pfund am 01.02.2002
  *
  *
  * 3 Ideen:
  * - 'später'
  * - Währung
  * - Betrag
  */

sealed trait Contract
final case class One(currency: Currency) extends Contract // Bsp: "Bekomme jetzt 1 EUR"
final case class Multiple(amount: Amount, contract: Contract)
    extends Contract // Bsp: "Bekomme jetzt 100 Eur jetzt"  !!Selbstreferenz!! Contract anstatt Currency
final case class Later(date: Date, contract: Contract) extends Contract
// dreht Vertag um (im paper give (Seite 5))
final case class Pay(contract: Contract) extends Contract
// binärer Operator wie +, *, overlay (images)
final case class Both(contract1: Contract, contract2: Contract) extends Contract
// neutrales Element (empty contract)
case object Zero extends Contract

object Contract {
  type Amount = Double

  // definiere Vertag als Funktion
  def zeroCouponBond(amount: Amount, currency: Currency, date: Date): Contract = Later(date, Multiple(amount, One(currency)))

  val contract1: Contract = Multiple(100, One(Currency.EUR)) // Bekomme 100 Eur jetzt
  val contract2: Contract = Later(
    Date("2001-01-29"),
    Multiple(100, One(Currency.GBP))
  )
  val contract3: Contract =
    Multiple(100, Later(Date("2001-01-29"), One(Currency.GBP))) // gleiche Semantik wie contract 2 (aber nicht gleich)

  val contract4: Contract = Pay(
    Later(
      Date("2002-02-01"),
      Multiple(100, One(Currency.GBP))
    )
  ) //  Pay(Pay(c)) ähnlich wie c

  val zcb1: Contract = zeroCouponBond(100, Currency.GBP, Date("2001-01-29"))

  // D1 aus dem paper pj-eber.pdf
  val contract5: Contract = Both(contract2, contract4)

  // ------------------

  sealed trait Direction
  case object Long extends Direction // bekommen
  case object Short extends Direction // bezahlen

  case class Payment(direction: Direction, date: Date, amount: Amount, currency: Currency)
  // operationelle Semantik: zeitliche Entwicklung der Domänobjecte
  // (vs. denotationelle Semantik: Domänobject auf mathematisches Objekt abbilden)

  // Zahlungen bis now
  def semantics(contract: Contract, now: Date): (Seq[Payment], Contract) =
    contract match {
      case Zero => (Seq.empty, Zero)
      case One(currency) => (Seq(Payment(Long, now, 1, currency)), Zero)
      case Multiple(amount, contract) =>
      case Later(date, contract) =>
      case Pay(contract) =>
      case Both(contract1, contract2) =>
    }

}
