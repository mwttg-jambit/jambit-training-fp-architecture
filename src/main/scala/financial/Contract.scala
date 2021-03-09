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

  //smart Constructor  // spart pattern matching für Zero in #semantics
  def multiple(amount: Amount, contract: Contract): Contract =
    contract match {
      case Zero => Zero
      case _ =>  Multiple(amount, contract)
    }

  // def both ...





  sealed trait Direction
  case object Long extends Direction // bekommen
  case object Short extends Direction // bezahlen

  case class Payment(direction: Direction, date: Date, amount: Amount, currency: Currency) {
    def multiply(factor: Amount): Payment =
      this.copy(amount = this.amount * factor)
  }
  // operationelle Semantik: zeitliche Entwicklung der Domänobjecte
  // (vs. denotationelle Semantik: Domänobject auf mathematisches Objekt abbilden)

  // Zahlungen bis now
  def semantics(originalContract: Contract, now: Date): (Seq[Payment], Contract) =
    originalContract match {
      case Zero          => (Seq.empty, Zero)
      case One(currency) => (Seq(Payment(Long, now, 1, currency)), Zero)
      case Multiple(amount, contract) =>
        val (payments, rest) = semantics(contract, now)
        rest match {
          case Zero => (payments.map(payment => payment.multiply(amount)), Zero)
          case _    => (payments.map(payment => payment.multiply(amount)), Multiple(amount, rest))
        }
      case Later(date, contract) =>
        if (now.isAfter(date))
          semantics(contract, now)
        else
          (Seq.empty, originalContract)
      case Pay(contract) =>
        val (payments, rest) = semantics(contract, now)
        val newPayments = payments.map(payment =>
          if (payment.direction == Short) payment.copy(direction = Long) else payment.copy(direction = Short)
        )

        rest match {
          case Zero => (newPayments, Zero)
          case _    => (newPayments, Pay(rest))
        }
      case Both(contract1, contract2) =>
        val (payments1, rest1) = semantics(contract1, now)
        val (payments2, rest2) = semantics(contract2, now)
        val newPayments        = payments1 :++ payments2

        (rest1, rest2) match {
          case (Zero, _) => (newPayments, rest2)
          case (_, Zero) => (newPayments, rest1)
          case (_, _)    => (newPayments, Both(rest1, rest2))
        }
    }
}


