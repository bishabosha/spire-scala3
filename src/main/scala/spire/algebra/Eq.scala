package spire
package algebra

import scala.{specialized => sp}

/**
 * A type class used to determine equality between 2 instances of the same
 * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
 * Moreover, `eqv` should form an equivalence relation.
 */
trait Eq[@sp A] { self =>

  /**
   * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
   */
  def eqv(x: A, y: A): Boolean

  /**
   * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
   */
  def neqv(x: A, y: A): Boolean = !eqv(x, y)
}

abstract class EqFunctions[E[T] <: Eq[T]] {

  def eqv[@sp A](x: A, y: A) given (ev: E[A]): Boolean =
    ev.eqv(x, y)

  def neqv[@sp A](x: A, y: A) given (ev: E[A]): Boolean =
    ev.neqv(x, y)

}

object Eq extends EqFunctions[Eq] {

  /**
   * Access an implicit `Eq[A]`.
   */
  inline final def apply[A] given (ev: Eq[A]): Eq[A] = ev

  /**
   * Convert an implicit `Eq[B]` to an `Eq[A]` using the given
   * function `f`.
   */
  def by[@sp A, @sp B](f: A => B) given (ev: Eq[B]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = ev.eqv(f(x), f(y))
    }

  /**
   * Create an `Eq` instance from an `eqv` implementation.
   */
  def instance[A](f: (A, A) => Boolean): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = f(x, y)
    }

  /**
   * An `Eq[A]` that delegates to universal equality (`==`).
   *
   * This can be useful for case classes, which have reasonable `equals`
   * implementations
   */
  def fromUniversalEquals[A]: Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = x == y
    }

}
