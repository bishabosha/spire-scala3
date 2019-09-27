package spire
package syntax

import spire.algebra._

trait EqSyntax {
  given eqSyntax[A:Eq]: {
    inline def (lhs:A) === [B] (rhs: B) (given ev1: B =:= A): Boolean = Eq[A].eqv(lhs, ev1(rhs))
    inline def (lhs:A) =!= [B] (rhs: B) (given ev1: B =:= A): Boolean = Eq[A].neqv(lhs, ev1(rhs))
  }
}

trait PartialOrderSyntax extends EqSyntax {
  given partialorderSyntax[A:PartialOrder]: {
    inline def (lhs: A) >  (rhs: A): Boolean = PartialOrder[A].gt(lhs, rhs)
    inline def (lhs: A) >= (rhs: A): Boolean = PartialOrder[A].gteqv(lhs, rhs)
    inline def (lhs: A) <  (rhs: A): Boolean = PartialOrder[A].lt(lhs, rhs)
    inline def (lhs: A) <= (rhs: A): Boolean = PartialOrder[A].lteqv(lhs, rhs)

    inline def (lhs: A) partialCompare (rhs: A): Double      = PartialOrder[A].partialCompare(lhs, rhs)
    inline def (lhs: A) tryCompare     (rhs: A): Option[Int] = PartialOrder[A].tryCompare(lhs, rhs)
  }
}

trait OrderSyntax extends PartialOrderSyntax {
  given orderSyntax[A:Order]: {
    inline def (lhs: A) compare(rhs: A): Int = Order[A].compare(lhs, rhs)
  }
}

trait CRigSyntax {
  given crigSyntax[A:CRig]: {
    inline def (lhs: A) +      (rhs: A)   : A       = CRig[A].plus(lhs, rhs)
    inline def (lhs: A) isZero (given Eq[A]): Boolean = CRig[A].isZero(lhs)
    inline def (lhs: A) *      (rhs: A)   : A       = CRig[A].times(lhs, rhs)
    inline def (lhs: A) isOne  (given Eq[A]): Boolean = CRig[A].isOne(lhs)
  }
}

trait CRingSyntax extends CRigSyntax {
  given cringSyntax[A:CRing]: {
    inline def (lhs: A) unary_-   : A = CRing[A].negate(lhs)
    inline def (lhs: A) - (rhs: A): A = CRing[A].minus(lhs, rhs)
  }
}

trait FieldSyntax extends CRingSyntax {
  given fieldSyntax[A:Field]: {
    inline def (lhs: A) reciprocal(): A = Field[A].reciprocal(lhs)
    inline def (lhs: A) / (rhs: A)  : A = Field[A].div(lhs, rhs)
  }
}

trait CforSyntax {
  import macros._
  import collection.immutable.NumericRange

  final type RangeLike = Range | NumericRange[Long]

  final type RangeElem[X <: RangeLike] = X match {
    case Range              => Int
    case NumericRange[Long] => Long
  }

  inline def cfor[A](init: => A)(test: => A => Boolean, next: => A => A)(body: => A => Unit): Unit =
    cforInline(init, test, next, body)

  inline def cforRange[R <: RangeLike](r: => R)(body: => RangeElem[R] => Unit): Unit =
    ${ cforRangeMacroGen('r, 'body) }

  inline def cforRange2[R <: RangeLike](r1: => R, r2: => R)(body: => (RangeElem[R], RangeElem[R]) => Unit): Unit =
    cforRange(r1) { x => cforRange(r2) { y => body(x, y) } }

  /** Alias of [[cforRange]] as an infix method.
   */
  inline def (r: => R) peek [R <: RangeLike](body: => RangeElem[R] => Unit): Unit =
    cforRange(r)(body)
}

trait AllSyntax extends
  CforSyntax with
  EqSyntax with
  PartialOrderSyntax with
  OrderSyntax with
  CRigSyntax with
  CRingSyntax with
  FieldSyntax
