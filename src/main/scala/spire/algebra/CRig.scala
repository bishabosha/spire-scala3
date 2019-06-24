package spire
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

/**
  * Typeclass for number types that implement addition and multiplication, with the
  * availability of values that behave like zero and one.
  */
trait CRig[@sp(Int, Long, Float, Double) A] {

  // Abstract members to implement

  def plus(x: A, y: A): A

  def times(x: A, y: A): A

  def zero: A

  def one: A

  /**
    * Tests if `a` is zero.
    */
  def isZero(a: A) given (ev: Eq[A]): Boolean = ev.eqv(a, zero)

  /**
    * Tests if `a` is one.
    */
  def isOne(a: A) given (ev: Eq[A]): Boolean = ev.eqv(a, one)

  // Additional methods

  def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else if (n == 0) zero
    else throw new IllegalArgumentException("Illegal negative exponent to sumN: %s" format n)

  protected[this] def positiveSumN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) plus(b, extra) else {
        val x = if ((k & 1) == 1) plus(b, extra) else extra
        loop(plus(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

}

trait CRigFunctions[C[T] <: CRig[T]] {

  def plus[@sp(Int, Long, Float, Double) A](x: A, y: A) given (ev: C[A]): A =
    ev.plus(x, y)

  def times[@sp(Int, Long, Float, Double) A](x: A, y: A) given (ev: C[A]): A =
    ev.times(x, y)

  def zero[@sp(Int, Long, Float, Double) A] given (ev: C[A]): A =
    ev.zero

  def one[@sp(Int, Long, Float, Double) A] given (ev: C[A]): A =
    ev.one

  def isZero[@sp(Int, Long, Float, Double) A](a: A) given (ev0: C[A], ev1: Eq[A]): Boolean =
    ev0.isZero(a)

  def isOne[@sp(Int, Long, Float, Double) A](a: A) given (ev0: C[A], ev1: Eq[A]): Boolean =
    ev0.isOne(a)

  def sumN[@sp(Int, Long, Float, Double) A](a: A, n: Int) given (ev: C[A]): A =
    ev.sumN(a, n)

}

object CRig extends CRigFunctions[CRig] {
  inline final def apply[A] given CRig[A]: CRig[A] = the[CRig[A]]
}
