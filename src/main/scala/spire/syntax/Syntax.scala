package spire
package syntax

import spire.algebra._

trait EqSyntax {
  delegate eqSyntax [A:Eq] {
    inline def (lhs:A) === [B] (rhs: B) given (ev1: B =:= A): Boolean = Eq[A].eqv(lhs, ev1(rhs))
    inline def (lhs:A) =!= [B] (rhs: B) given (ev1: B =:= A): Boolean = Eq[A].neqv(lhs, ev1(rhs))
  }
}

trait PartialOrderSyntax extends EqSyntax {
  delegate partialorderSyntax [A:PartialOrder] {
    inline def (lhs: A) >  (rhs: A): Boolean = PartialOrder[A].gt(lhs, rhs)
    inline def (lhs: A) >= (rhs: A): Boolean = PartialOrder[A].gteqv(lhs, rhs)
    inline def (lhs: A) <  (rhs: A): Boolean = PartialOrder[A].lt(lhs, rhs)
    inline def (lhs: A) <= (rhs: A): Boolean = PartialOrder[A].lteqv(lhs, rhs)

    inline def (lhs: A) partialCompare (rhs: A): Double      = PartialOrder[A].partialCompare(lhs, rhs)
    inline def (lhs: A) tryCompare     (rhs: A): Option[Int] = PartialOrder[A].tryCompare(lhs, rhs)
  }
}

trait OrderSyntax extends PartialOrderSyntax {
  delegate orderSyntax [A:Order] {
    inline def (lhs: A) compare(rhs: A): Int = Order[A].compare(lhs, rhs)
  }
}

trait CRigSyntax {
  delegate crigSyntax [A:CRig] {
    inline def (lhs: A) +      (rhs: A)   : A       = CRig[A].plus(lhs, rhs)
    inline def (lhs: A) isZero given Eq[A]: Boolean = CRig[A].isZero(lhs)
    inline def (lhs: A) *      (rhs: A)   : A       = CRig[A].times(lhs, rhs)
    inline def (lhs: A) isOne  given Eq[A]: Boolean = CRig[A].isOne(lhs)
  }
}

trait CRingSyntax extends CRigSyntax {
  delegate cringSyntax [A:CRing] {
    inline def (lhs: A) unary_-   : A = CRing[A].negate(lhs)
    inline def (lhs: A) - (rhs: A): A = CRing[A].minus(lhs, rhs)
  }
}

trait FieldSyntax extends CRingSyntax {
  delegate fieldSyntax [A:Field] {
    inline def (lhs: A) reciprocal(): A = Field[A].reciprocal(lhs)
    inline def (lhs: A) / (rhs: A)  : A = Field[A].div(lhs, rhs)
  }
}

trait CforSyntax {
  import macros._
  import collection.immutable.NumericRange

  inline def cfor[A](init: => A)(test: => A => Boolean, next: => A => A)(body: => A => Unit): Unit =
    cforInline(init, test, next, body)

  inline def cforRange[A <: RangeLike](r: => A)(body: => RangeElem[A] => Unit): Unit =
    ${ cforRangeMacroGen('r, 'body) }

  inline def cforRange2[A <: RangeLike](r1: => A, r2: => A)(body: => (RangeElem[A], RangeElem[A]) => Unit): Unit =
    cforRange(r1) { x => cforRange(r2) { y => body(x, y) } }
}

trait AllSyntax extends
  CforSyntax with
  EqSyntax with
  PartialOrderSyntax with
  OrderSyntax with
  CRigSyntax with
  CRingSyntax with
  FieldSyntax
