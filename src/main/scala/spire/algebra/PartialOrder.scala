package spire
package algebra

import java.lang.Double.isNaN
import scala.{specialized => sp}

/**
 * The `PartialOrder` type class is used to define a partial ordering on some type `A`.
 *
 * A partial order is defined by a relation <=, which obeys the following laws:
 *
 * - x <= x (reflexivity)
 * - if x <= y and y <= x, then x = y (anti-symmetry)
 * - if x <= y and y <= z, then x <= z (transitivity)
 *
 * To compute both <= and >= at the same time, we use a Double number
 * to encode the result of the comparisons x <= y and x >= y.
 * The truth table is defined as follows:
 *
 * x <= y    x >= y      Double
 * true      true        = 0.0     (corresponds to x = y)
 * false     false       = NaN     (x and y cannot be compared)
 * true      false       = -1.0    (corresponds to x < y)
 * false     true        = 1.0     (corresponds to x > y)
 */
trait PartialOrder[@sp A] extends Eq[A] { self =>

  /**
   * Result of comparing `x` with `y`. Returns NaN if operands are not
   * comparable. If operands are comparable, returns a Double whose
   * sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */
  def partialCompare(x: A, y: A): Double

  /**
   * Result of comparing `x` with `y`. Returns None if operands are
   * not comparable. If operands are comparable, returns Some[Int]
   * where the Int sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */
  def tryCompare(x: A, y: A): Option[Int] = {
    val c = partialCompare(x, y)
    if (isNaN(c)) None else Some(c.sign.toInt)
  }

  // The following may be overridden for performance:

  /**
   * Returns true if `x` = `y`, false otherwise.
   */
  def eqv(x: A, y: A): Boolean = partialCompare(x, y) == 0

  /**
   * Returns true if `x` <= `y`, false otherwise.
   */
  def lteqv(x: A, y: A): Boolean = partialCompare(x, y) <= 0

  /**
   * Returns true if `x` < `y`, false otherwise.
   */
  def lt(x: A, y: A): Boolean = partialCompare(x, y) < 0

  /**
   * Returns true if `x` >= `y`, false otherwise.
   */
  def gteqv(x: A, y: A): Boolean = partialCompare(x, y) >= 0

  /**
   * Returns true if `x` > `y`, false otherwise.
   */
  def gt(x: A, y: A): Boolean = partialCompare(x, y) > 0
}

abstract class PartialOrderFunctions[P[T] <: PartialOrder[T]] extends EqFunctions[P] {

  def partialCompare[@sp A](x: A, y: A)(using ev: P[A]): Double =
    ev.partialCompare(x, y)
  def tryCompare[@sp A](x: A, y: A)(using ev: P[A]): Option[Int] =
    ev.tryCompare(x, y)

  def lteqv[@sp A](x: A, y: A)(using ev: P[A]): Boolean =
    ev.lteqv(x, y)
  def lt[@sp A](x: A, y: A)(using ev: P[A]): Boolean =
    ev.lt(x, y)
  def gteqv[@sp A](x: A, y: A)(using ev: P[A]): Boolean =
    ev.gteqv(x, y)
  def gt[@sp A](x: A, y: A)(using ev: P[A]): Boolean =
    ev.gt(x, y)

}

object PartialOrder extends PartialOrderFunctions[PartialOrder] {

  /**
   * Access a given `PartialOrder[A]`.
   */
  inline def apply[A](using PartialOrder[A]) = summon[PartialOrder[A]]

  /**
   * Convert a given `PartialOrder[B]` to an `PartialOrder[A]` using the given
   * function `f`.
   */
  def by[@sp A, @sp B](f: A => B)(using ev: PartialOrder[B]): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A): Double = ev.partialCompare(f(x), f(y))
    }

  /**
   * Define a `PartialOrder[A]` using the given function `f`.
   */
  def from[@sp A](f: (A, A) => Double): PartialOrder[A] =
    new PartialOrder[A] {
      def partialCompare(x: A, y: A) = f(x, y)
    }
}
