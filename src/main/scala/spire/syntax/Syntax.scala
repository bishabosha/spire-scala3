package spire
package syntax

import spire.algebra._

trait EqSyntax with
  given EqSyntax: [A: Eq, B](lhs: A)(given ev1: B =:= A) extended with
    inline def ===(rhs: B): Boolean = Eq[A].eqv(lhs, ev1(rhs))
    inline def =!=(rhs: B): Boolean = Eq[A].neqv(lhs, ev1(rhs))

trait PartialOrderSyntax extends EqSyntax
  given PartialOrderSyntax: [A: PartialOrder](lhs: A) extended with
    inline def >(rhs: A): Boolean = PartialOrder[A].gt(lhs, rhs)
    inline def >=(rhs: A): Boolean = PartialOrder[A].gteqv(lhs, rhs)
    inline def <(rhs: A): Boolean = PartialOrder[A].lt(lhs, rhs)
    inline def <=(rhs: A): Boolean = PartialOrder[A].lteqv(lhs, rhs)

    inline def partialCompare(rhs: A): Double = PartialOrder[A].partialCompare(lhs, rhs)
    inline def tryCompare(rhs: A): Option[Int] = PartialOrder[A].tryCompare(lhs, rhs)

trait OrderSyntax extends PartialOrderSyntax
  given OrderSyntax: [A: Order](lhs: A) extended with
    inline def compare(rhs: A): Int = Order[A].compare(lhs, rhs)

trait CRigSyntax
  given CRigSyntax: [A: CRig](lhs: A) extended with
    inline def +(rhs: A): A = CRig[A].plus(lhs, rhs)
    inline def isZero(given Eq[A]): Boolean = CRig[A].isZero(lhs)
    inline def *(rhs: A): A = CRig[A].times(lhs, rhs)
    inline def isOne(given Eq[A]): Boolean = CRig[A].isOne(lhs)

trait CRingSyntax extends CRigSyntax
  given CRingSyntax: [A: CRing](lhs: A) extended with
    inline def unary_- : A = CRing[A].negate(lhs)
    inline def -(rhs: A): A = CRing[A].minus(lhs, rhs)

trait FieldSyntax extends CRingSyntax
  given FieldSyntax: [A: Field](lhs: A) extended with
    inline def reciprocal(): A = Field[A].reciprocal(lhs)
    inline def /(rhs: A): A = Field[A].div(lhs, rhs)

trait CforSyntax with
  import macros._
  import collection.immutable.NumericRange

  final type RangeLike = Range | NumericRange[Long]

  final type RangeElem[X <: RangeLike] = X match
    case Range              => Int
    case NumericRange[Long] => Long

  inline def cfor[A](init: => A)(test: => A => Boolean, next: => A => A)(body: => A => Unit): Unit =
    cforInline(init, test, next, body)

  inline def cforRange[R <: RangeLike](r: => R)(body: => RangeElem[R] => Unit): Unit =
    ${ cforRangeMacroGen('r, 'body) }

  inline def cforRange2[R <: RangeLike](r1: => R, r2: => R)(body: => (RangeElem[R], RangeElem[R]) => Unit): Unit =
    cforRange(r1) { x => cforRange(r2) { y => body(x, y) } }

  /** Alias of [[cforRange]] as an infix method.
   */
  inline def [R <: RangeLike](r: => R) peek(body: => RangeElem[R] => Unit): Unit =
    cforRange(r)(body)

trait AllSyntax extends CforSyntax
  with EqSyntax
  with PartialOrderSyntax
  with OrderSyntax
  with CRigSyntax
  with CRingSyntax
  with FieldSyntax
