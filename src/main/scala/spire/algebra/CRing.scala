package spire
package algebra

import scala.annotation.tailrec
import scala.{specialized => sp}

/**
  * Typeclass for number types that implement addition, subtraction and multiplication, with the
  * availability of values that behave like zero and one.
  */
trait CRing[@sp(Int, Long, Float, Double) A] extends CRig[A] {

  def negate(x: A): A

  def minus(x: A, y: A): A = plus(x, negate(y))

  override def sumN(a: A, n: Int): A =
    if (n > 0) positiveSumN(a, n)
    else if (n == 0) zero
    else if (n == Int.MinValue) positiveSumN(negate(plus(a, a)), 1073741824)
    else positiveSumN(negate(a), -n)

  /**
    * Convert the given integer to an instance of A.
    *
    * Defined to be equivalent to `sumN(one, n)`.
    *
    * That is, `n` repeated summations of this ring's `one`, or `-n`
    * summations of `-one` if `n` is negative.
    *
    * Most type class instances should consider overriding this method
    * for performance reasons.
    */
  def fromInt(n: Int): A = sumN(one, n)

  /**
    * Convert the given BigInt to an instance of A.
    *
    * This is equivalent to `n` repeated summations of this ring's `one`, or
    * `-n` summations of `-one` if `n` is negative.
    *
    * Most type class instances should consider overriding this method for
    * performance reasons.
    */
  def fromBigInt(n: BigInt): A =
    if (n.isValidInt) {
      fromInt(n.toInt)
    } else {
      val d = fromInt(1 << 30)
      val mask = (1L << 30) - 1
      @tailrec def loop(k: A, x: BigInt, acc: A): A =
        if (x.isValidInt) {
          plus(times(k, fromInt(x.toInt)), acc)
        } else {
          val y = x >> 30
          val r = fromInt((x & mask).toInt)
          loop(times(d, k), y, plus(times(k, r), acc))
        }

      val absValue = loop(one, n.abs, zero)
      if (n.signum < 0) negate(absValue) else absValue
    }

}

trait CRingFunctions[C[T] <: CRing[T]] extends CRigFunctions[C] {

  def negate[@sp(Int, Long, Float, Double) A](x: A) given (ev: C[A]): A =
    ev.negate(x)

  def minus[@sp(Int, Long, Float, Double) A](x: A, y: A) given (ev: C[A]): A =
    ev.minus(x, y)

  def fromInt[@sp(Int, Long, Float, Double) A](n: Int) given (ev: C[A]): A =
    ev.fromInt(n)

  def fromBigInt[@sp(Int, Long, Float, Double) A](n: BigInt) given (ev: C[A]): A =
    ev.fromBigInt(n)

}

object CRing extends CRingFunctions[CRing] {

  @inline final def apply[A] given (r: CRing[A]): CRing[A] = r

}
