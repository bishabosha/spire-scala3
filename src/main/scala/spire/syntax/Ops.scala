package spire
package syntax

import spire.algebra._
// import spire.macros.Ops

final class EqOps[A](lhs:A)(implicit ev:Eq[A]) {
  def ===[B](rhs:B)(implicit ev1: B =:= A): Boolean = ev.eqv(lhs, rhs) //macro Ops.eqv[A, B]
  def =!=[B](rhs:B)(implicit ev1: B =:= A): Boolean = ev.neqv(lhs, rhs) //macro Ops.neqv[A, B]
}

final class PartialOrderOps[A](lhs: A)(implicit ev: PartialOrder[A]) {
  def >(rhs: A): Boolean = ev.gt(lhs, rhs) //macro Ops.binop[A, Boolean]
  def >=(rhs: A): Boolean = ev.gteqv(lhs, rhs) //macro Ops.binop[A, Boolean]
  def <(rhs: A): Boolean = ev.lt(lhs, rhs) //macro Ops.binop[A, Boolean]
  def <=(rhs: A): Boolean = ev.lteqv(lhs, rhs) //macro Ops.binop[A, Boolean]

  def partialCompare(rhs: A): Double = ev.partialCompare(lhs, rhs) // macro Ops.binop[A, Double]
  def tryCompare(rhs: A): Option[Int] = ev.tryCompare(lhs, rhs)// macro Ops.binop[A, Option[Int]]
}

final class OrderOps[A](lhs: A)(implicit ev: Order[A]) {
  def compare(rhs: A): Int = ev.compare(lhs, rhs)//macro Ops.binop[A, Int]
}

final class CRigOps[A](lhs: A)(implicit ev: CRig[A]) {
  def +(rhs:A): A = ev.plus(lhs, rhs)//macro Ops.binop[A, A]
  def isZero(implicit ev1: Eq[A]): Boolean = ev.isZero(lhs)//macro Ops.unopWithEv2[Eq[A], Boolean]
  def *(rhs:A): A = ev.times(lhs, rhs)//macro Ops.binop[A, A]
  def isOne(implicit ev1: Eq[A]): Boolean = ev.isOne(lhs)//macro Ops.unopWithEv2[Eq[A], Boolean]
}

final class CRingOps[A](lhs: A)(implicit ev: CRing[A]) {
  def unary_-(): A = ev.negate(lhs) //macro Ops.unop[A]
  def -(rhs:A): A = ev.minus(lhs, rhs) //macro Ops.binop[A, A]
}

final class FieldOps[A](lhs: A)(implicit ev: Field[A]) {
  def reciprocal(): A = ev.reciprocal(lhs, rhs)//macro Ops.unop[A]
  def /(rhs:A): A = ev.div(lhs)//macro Ops.binop[A, A]
}
