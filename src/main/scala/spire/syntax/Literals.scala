package spire
package syntax

import spire.algebra.{Field, CRing}

import scala.quoted._
import scala.quoted.matching._
import scala.util.FromDigits

object primitives {

  inline def (n: => Int) as [A] (given A: CRing[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromInt(n)

  inline def (n: => Double) as [A] (given A: Field[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromDouble(n)

  private inline def ringFromBigInt[A](digits: String, radix: Int)(given A: CRing[A]) =
    A.fromBigInt(summon[FromDigits[BigInt]].fromDigits(digits, radix))

  private inline def fieldFromBigDecimal[A](digits: String)(given A: Field[A]) =
    A.fromBigDecimal(summon[FromDigits[BigDecimal]].fromDigits(digits))

  class RingLiteral[A](given CRing[A]) extends FromDigits.WithRadix[A]
    def fromDigits(digits: String, radix: Int): A = ringFromBigInt(digits, radix)

  class FieldLiteral[A](given Field[A]) extends FromDigits.Floating[A]
    def fromDigits(digits: String): A = fieldFromBigDecimal(digits)

  given [A](given ev: CRing[A]): RingLiteral[A]
    override inline def fromDigits(digits: String, radix: Int): A =
      ${ fromRingImpl('digits, 'radix, 'ev) }

  given [A](given ev: Field[A]): FieldLiteral[A]
    override inline def fromDigits(digits: String): A =
      ${ fromFieldImpl('digits, 'ev) }

  def fromRingImpl[A: Type](digits: Expr[String], radix: Expr[Int], ring: Expr[CRing[A]])(given qctx: QuoteContext): Expr[A] =
    (digits, radix) match
    case (Const(ds), Const(r)) =>
      try
        FromDigits.intFromDigits(ds, r) match
          case 0 => '{ $ring.zero }
          case 1 => '{ $ring.one }
          case n => '{ $ring.fromInt(${Expr(n)}) }
      catch
        case e: (FromDigits.NumberTooLarge | FromDigits.MalformedNumber) =>
          '{ ringFromBigInt($digits, $radix)(given $ring) }
    case _ => '{ ringFromBigInt($digits, $radix)(given $ring) }

  def fromFieldImpl[A: Type](digits: Expr[String], field: Expr[Field[A]])(given qctx: QuoteContext): Expr[A] =
    digits match
    case Const(ds) =>
      try
        FromDigits.intFromDigits(ds) match
          case 0 => '{ $field.zero }
          case 1 => '{ $field.one }
          case n => '{ $field.fromInt(${Expr(n)}) }
      catch
        case e: (FromDigits.NumberTooLarge | FromDigits.MalformedNumber) =>
          if """.*[.eE].*""".r.matches(ds) then
            if BigDecimal(ds).isDecimalDouble then
              FromDigits.doubleFromDigits(ds) match
                case 0.0 => '{ $field.zero }
                case 1.0 => '{ $field.one }
                case n   => '{ $field.fromDouble(${Expr(n)}) }
            else
              '{ fieldFromBigDecimal($digits)(given $field) }
          else
            '{ ringFromBigInt($digits, ${Expr(10)})(given $field) }

    case _ => '{ fieldFromBigDecimal($digits)(given $field) }

}
