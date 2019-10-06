package spire
package algebra

import scala.{ specialized => sp }
import java.math.BigDecimal.{valueOf => JBigDecimal}

trait Field[@sp(Int, Long, Float, Double) A] extends CRing[A] { self =>

  // To implement

  def div(x: A, y: A): A

  // Methods

  def reciprocal(x: A): A = div(one, x)

  def fromDouble(n: Double): A = {
    if n.isValidInt then {
      fromInt(n.toInt)
    }
    else if n.isWhole then {
      if (n < 0 && n >= Long.MinValue) || (n > 0 && n <= Long.MaxValue) then
        fromBigInt(BigInt(n.toLong))
      else
        fromBigInt(JBigDecimal(n).toBigInteger)
    }
    else {
      fromBigDecimal(BigDecimal(n))
    }
  }

  def fromBigDecimal(n: BigDecimal): A = {
    val quot     = fromBigInt(n.quot(Field.one).toBigInt)
    val remRaw   = n.remainder(Field.one)
    val unscaled = fromBigInt(remRaw.underlying.unscaledValue)
    val scalePow = fromBigInt(JBigDecimal(scala.math.pow(10.0, remRaw.scale.toDouble)).toBigInteger)
    plus(quot, div(unscaled, scalePow))
  }

}

trait FieldFunctions[F[T] <: Field[T]] extends CRingFunctions[F] {

  def reciprocal[@sp(Int, Long, Float, Double) A](x: A) (given ev: F[A]): A =
    ev.reciprocal(x)

  def div[@sp(Int, Long, Float, Double) A](x: A, y: A) (given ev: F[A]): A =
    ev.div(x, y)

}

object Field extends FieldFunctions[Field] {
  private val one: BigDecimal = 1
  private val two: BigDecimal = 2
  inline def apply[A] (given Field[A]) = summon[Field[A]]
}
