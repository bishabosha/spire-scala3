package spire.syntax

import spire.algebra.{Field, CRing}

import scala.quoted._

import scala.util.FromDigits

object literals {

  given [A](using ev: CRing[A]): RadixLiteral[A] with
    override inline def fromDigits(digits: String): A =
      ${ macros.fromRingImpl('digits, '{10}, 'ev) }
    override inline def fromDigits(digits: String, radix: Int): A =
      ${ macros.fromRingImpl('digits, 'radix, 'ev) }

  given [A](using ev: Field[A]): FloatingLiteral[A] with
    override inline def fromDigits(digits: String): A =
      ${ macros.fromFieldImpl('digits, 'ev) }

  class RadixLiteral[A](using A: CRing[A]) extends FromDigits.WithRadix[A]:
    override def fromDigits(digits: String): A = fromDigits(digits, 10)
    override def fromDigits(digits: String, radix: Int): A = A.fromBigInt(BigInt(digits, radix))

  class FloatingLiteral[A](using A: Field[A]) extends FromDigits.Floating[A]:
    override def fromDigits(digits: String): A = A.fromBigDecimal(BigDecimal(digits))

}
