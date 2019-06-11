package spire
package algebra

import scala.{specialized => sp}

/**
 * The `Order` type class is used to define a total ordering on some type `A`.
 * An order is defined by a relation <=, which obeys the following laws:
 *
 * - either x <= y or y <= x (totality)
 * - if x <= y and y <= x, then x == y (antisymmetry)
 * - if x <= y and y <= z, then x <= z (transitivity)
 *
 * The truth table for compare is defined as follows:
 *
 * x <= y    x >= y      Int
 * true      true        = 0     (corresponds to x == y)
 * true      false       < 0     (corresponds to x < y)
 * false     true        > 0     (corresponds to x > y)
 *
 * By the totality law, x <= y and y <= x cannot be both false.
 */
trait Order[@sp A] extends PartialOrder[A] { self =>

  /**
   * Result of comparing `x` with `y`. Returns an Int whose sign is:
   * - negative iff `x < y`
   * - zero     iff `x = y`
   * - positive iff `x > y`
   */
  def compare(x: A, y: A): Int

  def partialCompare(x: A, y: A): Double = compare(x, y).toDouble

  // The following may be overridden for performance:

  /**
   * Returns true if `x` = `y`, false otherwise.
   */
  override def eqv(x: A, y: A): Boolean =
    compare(x, y) == 0

  /**
   * Returns true if `x` != `y`, false otherwise.
   *
   * Note: this default implementation provided by [[Order]] is the same as the
   * one defined in [[Eq]], but for purposes of binary compatibility, the
   * override in [[Order]] has not yet been removed.
   * See [[https://github.com/typelevel/cats/pull/2230#issuecomment-381818633 this discussion]].
   */
  override def neqv(x: A, y: A): Boolean = !eqv(x, y)

  /**
   * Returns true if `x` <= `y`, false otherwise.
   */
  override def lteqv(x: A, y: A): Boolean =
    compare(x, y) <= 0

  /**
   * Returns true if `x` < `y`, false otherwise.
   */
  override def lt(x: A, y: A): Boolean =
    compare(x, y) < 0

  /**
   * Returns true if `x` >= `y`, false otherwise.
   */
  override def gteqv(x: A, y: A): Boolean =
    compare(x, y) >= 0

  /**
   * Returns true if `x` > `y`, false otherwise.
   */
  override def gt(x: A, y: A): Boolean =
    compare(x, y) > 0
}

abstract class OrderFunctions[O[T] <: Order[T]] extends PartialOrderFunctions[O] {

  def compare[@sp A](x: A, y: A) given (ev: O[A]): Int =
    ev.compare(x, y)

}

object Order extends OrderFunctions[Order] {

  /**
   * Access an implicit `Order[A]`.
   */
  inline final def apply[A] given (ev: Order[A]): Order[A] = ev

  /**
   * Convert an implicit `Order[B]` to an `Order[A]` using the given
   * function `f`.
   */
  def by[@sp A, @sp B](f: A => B) given (ev: Order[B]): Order[A] =
    new Order[A] {
      def compare(x: A, y: A): Int = ev.compare(f(x), f(y))
    }

  /**
   * Define an `Order[A]` using the given function `f`.
   */
  def from[@sp A](f: (A, A) => Int): Order[A] =
    new Order[A] {
      def compare(x: A, y: A) = f(x, y)
    }

}
