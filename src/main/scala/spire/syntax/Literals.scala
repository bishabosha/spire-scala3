package spire.syntax

import spire.algebra.{Field, CRing}
import spire.syntax.macros.{fromFieldImpl, fromRingImpl}

import scala.quoted._
import scala.util.FromDigits

object literals {

  given [A](using ev: CRing[A]) as FromDigits.WithRadix[A]:
    override inline def fromDigits(digits: String): A =
      ${ fromRingImpl('digits, Expr(10), 'ev) }
    override inline def fromDigits(digits: String, radix: Int): A =
      ${ fromRingImpl('digits, 'radix, 'ev) }

  given [A](using ev: Field[A]) as FromDigits.Floating[A]:
    override inline def fromDigits(digits: String): A =
      ${ fromFieldImpl('digits, 'ev) }

}
