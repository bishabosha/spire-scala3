package spire.syntax

import spire.algebra.{Field, CRing}

import scala.quoted._
import scala.quoted.matching._
import scala.util.FromDigits

object literals {

  given [A](given ev: CRing[A]): RadixLiteral[A]
    override inline def fromDigits(digits: String): A =
      ${ fromRingImpl('digits, Expr(10), 'ev) }
    override inline def fromDigits(digits: String, radix: Int): A =
      ${ fromRingImpl('digits, 'radix, 'ev) }

  given [A](given ev: Field[A]): FloatingLiteral[A]
    override inline def fromDigits(digits: String): A =
      ${ fromFieldImpl('digits, 'ev) }

  private def fromRingImpl[A: Type](digits: Expr[String], radix: Expr[Int], A: Expr[CRing[A]])(given QuoteContext): Expr[A] =
    digits -> radix match
    case Const(ds) -> Const(r) => fromBigIntImpl(BigInt(ds, r), A)
    case _                     => '{ $A.fromBigInt(BigInt($digits, $radix)) }

  private def fromFieldImpl[A: Type](digits: Expr[String], A: Expr[Field[A]])(given QuoteContext): Expr[A] =
    digits match
    case Const(ds) =>
      if floating.matches(ds) then
        val bigdec = BigDecimal(ds)
        if bigdec.isDecimalDouble || bigdec.isBinaryDouble || bigdec.isExactDouble then bigdec.toDouble match
          case 0.0 => '{ $A.zero                     }
          case 1.0 => '{ $A.one                      }
          case n   => '{ $A.fromDouble(${ Expr(n) }) }
        else
          '{ $A.fromBigDecimal(${ Expr(bigdec) }) }
      else
        fromBigIntImpl(BigInt(ds), A)

    case _ => '{ $A.fromBigDecimal(BigDecimal($digits)) }

  private def fromBigIntImpl[A: Type](bigint: BigInt, A: Expr[CRing[A]])(given QuoteContext): Expr[A] =
    if bigint.isValidInt then bigint.toInt match
      case 0 => '{ $A.zero                  }
      case 1 => '{ $A.one                   }
      case n => '{ $A.fromInt(${ Expr(n) }) }
    else '{ $A.fromBigInt(${ Expr(bigint) }) }

  private val floating = """.*[.eE].*""".r

  class RadixLiteral[A](given A: CRing[A]) extends FromDigits.WithRadix[A]
    def fromDigits(digits: String, radix: Int): A = A.fromBigInt(BigInt(digits, radix))

  class FloatingLiteral[A](given A: Field[A]) extends FromDigits.Floating[A]
    def fromDigits(digits: String): A = A.fromBigDecimal(BigDecimal(digits))

}
