package spire
package syntax

import spire.algebra._
import spire.macros.Syntax

trait EqSyntax {
  implicit def eqOps[A:Eq](a:A): EqOps[A] = new EqOps(a)
}

trait PartialOrderSyntax extends EqSyntax {
  implicit def partialOrderOps[A:PartialOrder](a:A): PartialOrderOps[A] = new PartialOrderOps(a)
}

trait OrderSyntax extends PartialOrderSyntax {
  implicit def orderOps[A:Order](a:A): OrderOps[A] = new OrderOps(a)
}

trait CRigSyntax {
  implicit def cRigOps[A:CRig](a:A): CRigOps[A] = new CRigOps(a)
}

trait CRingSyntax extends CRigSyntax {
  implicit def cRingOps[A:CRing](a:A): CRingOps[A] = new CRingOps(a)
}

trait FieldSyntax extends CRingSyntax {
  implicit def fieldOps[A:Field](a:A): FieldOps[A] = new FieldOps(a)
}

trait CforSyntax {
  def cfor[A](init:A)(test:A => Boolean, next:A => A)(body:A => Unit): Unit =
    macro Syntax.cforMacro[A]
  def cforRange(r: Range)(body: Int => Unit): Unit =
    macro Syntax.cforRangeMacro
  def cforRange2(r1: Range, r2: Range)(body: (Int, Int) => Unit): Unit =
    macro Syntax.cforRange2Macro
}

trait AllSyntax extends
    CforSyntax with
    EqSyntax with
    PartialOrderSyntax with
    OrderSyntax with
    CRigSyntax with
    CRingSyntax with
    FieldSyntax
