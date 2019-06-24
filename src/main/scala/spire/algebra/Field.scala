package spire
package algebra

import scala.{ specialized => sp }

trait Field[@sp(Int, Long, Float, Double) A] extends CRing[A] { self =>

  // To implement

  def div(x: A, y: A): A

  // Methods

  def reciprocal(x: A): A = div(one, x)

}

trait FieldFunctions[F[T] <: Field[T]] extends CRingFunctions[F] {

  def reciprocal[@sp(Int, Long, Float, Double) A](x: A) given (ev: F[A]): A =
    ev.reciprocal(x)

  def div[@sp(Int, Long, Float, Double) A](x: A, y: A) given (ev: F[A]): A =
    ev.div(x, y)

}

object Field extends FieldFunctions[Field] {
  inline final def apply[A] given Field[A]: Field[A] = the[Field[A]]
}
