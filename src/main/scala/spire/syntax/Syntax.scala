package spire
package syntax

import spire.algebra._

trait EqSyntax {
  delegate eqSyntax [A:Eq] {
    inline def (lhs:A) === [B] (rhs:B)(implicit ev1: B =:= A): Boolean = the[Eq[A]].eqv(lhs, ev1(rhs))
    inline def (lhs:A) =!= [B] (rhs:B)(implicit ev1: B =:= A): Boolean = the[Eq[A]].neqv(lhs, ev1(rhs))
  }
}

trait PartialOrderSyntax extends EqSyntax {
  delegate partialorderSyntax [A:PartialOrder] {
    inline def (lhs: A) > (rhs: A): Boolean = the[PartialOrder[A]].gt(lhs, rhs)
    inline def (lhs: A) >= (rhs: A): Boolean = the[PartialOrder[A]].gteqv(lhs, rhs)
    inline def (lhs: A) < (rhs: A): Boolean = the[PartialOrder[A]].lt(lhs, rhs)
    inline def (lhs: A) <= (rhs: A): Boolean = the[PartialOrder[A]].lteqv(lhs, rhs)

    inline def (lhs: A) partialCompare(rhs: A): Double = the[PartialOrder[A]].partialCompare(lhs, rhs)
    inline def (lhs: A) tryCompare(rhs: A): Option[Int] = the[PartialOrder[A]].tryCompare(lhs, rhs)
  }
}

trait OrderSyntax extends PartialOrderSyntax {
  delegate orderSyntax [A:Order] {
    inline def (lhs: A) compare(rhs: A): Int = the[Order[A]].compare(lhs, rhs)
  }
}

trait CRigSyntax {
  delegate crigSyntax [A:CRig] {
    inline def (lhs: A) +(rhs:A): A = the[CRig[A]].plus(lhs, rhs)
    inline def (lhs: A) isZero(implicit ev1: Eq[A]): Boolean = the[CRig[A]].isZero(lhs)
    inline def (lhs: A) *(rhs:A): A = the[CRig[A]].times(lhs, rhs)
    inline def (lhs: A) isOne(implicit ev1: Eq[A]): Boolean = the[CRig[A]].isOne(lhs)
  }
}

trait CRingSyntax extends CRigSyntax {
  delegate cringSyntax [A:CRing] {
    inline def (lhs: A) unary_- : A = the[CRing[A]].negate(lhs)
    inline def (lhs: A) -(rhs:A): A = the[CRing[A]].minus(lhs, rhs)
  }
}

trait FieldSyntax extends CRingSyntax {
  delegate [A:Field] {
    inline def (lhs: A) reciprocal(): A = the[Field[A]].reciprocal(lhs)
    inline def (lhs: A) /[A:Field] (rhs:A): A = the[Field[A]].div(lhs, rhs)
  }
}

trait CforSyntax {
  import macros._

  inline def cfor[A](init: A)(test: => A => Boolean, next: => A => A)(body: => A => Unit): Unit =
    ${ cforMacro('init, 'test, 'next, 'body) }

  // def cforRange(r: Range)(body: Int => Unit): Unit =
  //   macro Syntax.cforRangeMacro
  // def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit =
  //   macro Syntax.cforRange2Macro
}

trait AllSyntax extends
  CforSyntax with
  EqSyntax with
  PartialOrderSyntax with
  OrderSyntax with
  CRigSyntax with
  CRingSyntax with
  FieldSyntax
