package spire
package syntax

import spire.algebra._

trait EqSyntax:
  given EqOps: AnyRef with
    extension [A: Eq, B](lhs: A)(using ev1: B =:= A)
      inline def ===(rhs: B): Boolean = Eq[A].eqv(lhs, ev1(rhs))
      inline def =!=(rhs: B): Boolean = Eq[A].neqv(lhs, ev1(rhs))

trait PartialOrderSyntax extends EqSyntax:
  given PartialOrderOps: AnyRef with
    extension [A: PartialOrder](lhs: A)
      inline def >(rhs: A): Boolean = PartialOrder[A].gt(lhs, rhs)
      inline def >=(rhs: A): Boolean = PartialOrder[A].gteqv(lhs, rhs)
      inline def <(rhs: A): Boolean = PartialOrder[A].lt(lhs, rhs)
      inline def <=(rhs: A): Boolean = PartialOrder[A].lteqv(lhs, rhs)

      inline def partialCompare(rhs: A): Double = PartialOrder[A].partialCompare(lhs, rhs)
      inline def tryCompare(rhs: A): Option[Int] = PartialOrder[A].tryCompare(lhs, rhs)

trait OrderSyntax extends PartialOrderSyntax:
  given OrderOps: AnyRef with
    extension [A: Order](lhs: A)
      inline def compare(rhs: A): Int = Order[A].compare(lhs, rhs)

trait CRigSyntax:
  given CRigOps: AnyRef with
    extension [A: CRig](lhs: A)
      inline def +(rhs: A): A = CRig[A].plus(lhs, rhs)
      inline def isZero(using Eq[A]): Boolean = CRig[A].isZero(lhs)
      inline def *(rhs: A): A = CRig[A].times(lhs, rhs)
      inline def isOne(using Eq[A]): Boolean = CRig[A].isOne(lhs)

trait CRingSyntax extends CRigSyntax:
  given CRingOps: AnyRef with
    extension [A: CRing](lhs: A)
      inline def unary_- : A = CRing[A].negate(lhs)
      inline def -(rhs: A): A = CRing[A].minus(lhs, rhs)

trait FieldSyntax extends CRingSyntax:
  given FieldOps: AnyRef with
    extension [A: Field](lhs: A)
      inline def reciprocal(): A = Field[A].reciprocal(lhs)
      inline def /(rhs: A): A = Field[A].div(lhs, rhs)

trait CforSyntax:
  import macros._
  import collection.immutable.NumericRange

  final type RangeLike = Range | NumericRange[Long]

  final type RangeElem[X <: RangeLike] = X match
    case Range              => Int
    case NumericRange[Long] => Long

  inline def cfor[A](inline init: A)(inline test: A => Boolean, inline next: A => A)(inline body: A => Unit): Unit =
    cforInline(init, test, next, body)

  inline def cforRange[R <: RangeLike](inline r: R)(inline body: RangeElem[R] => Unit): Unit =
    ${ cforRangeMacroGen('r, 'body) }

  inline def cforRange2[R <: RangeLike](inline r1: R, inline r2: R)(inline body: (RangeElem[R], RangeElem[R]) => Unit): Unit =
    cforRange(r1) { x => cforRange(r2) { y => body(x, y) } }

  given CforOps: AnyRef with
    /** Alias of [[cforRange]] as an infix method.
     */
    extension [R <: RangeLike](inline r: R) inline def peek(inline body: RangeElem[R] => Unit): Unit =
      cforRange(r)(body)

trait AllSyntax extends CforSyntax
  with EqSyntax
  with PartialOrderSyntax
  with OrderSyntax
  with CRigSyntax
  with CRingSyntax
  with FieldSyntax
