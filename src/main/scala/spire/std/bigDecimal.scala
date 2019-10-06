package spire.std

import java.math.MathContext

import spire.algebra.{Field, Order}

class BigDecimalAlgebra extends Order[BigDecimal] with Field[BigDecimal] {

  // Order

  override def eqv(x: BigDecimal, y: BigDecimal): Boolean = x == y
  override def neqv(x: BigDecimal, y: BigDecimal): Boolean = x != y
  override def gt(x: BigDecimal, y: BigDecimal): Boolean = x > y
  override def gteqv(x: BigDecimal, y: BigDecimal): Boolean = x >= y
  override def lt(x: BigDecimal, y: BigDecimal): Boolean = x < y
  override def lteqv(x: BigDecimal, y: BigDecimal): Boolean = x <= y

  // Scala compareTo has no guarantee to return only -1, 0 or 1 as per Spire compare contract,
  // so we call Java's compareTo which does
  def compare(x: BigDecimal, y: BigDecimal): Int = x.bigDecimal.compareTo(y.bigDecimal)

  // Field

  override def minus(a: BigDecimal, b: BigDecimal): BigDecimal = a - b
  def negate(a: BigDecimal): BigDecimal = -a
  val one: BigDecimal = BigDecimal(1.0)
  def plus(a: BigDecimal, b: BigDecimal): BigDecimal = a + b
  override def times(a: BigDecimal, b: BigDecimal): BigDecimal = a * b
  val zero: BigDecimal = BigDecimal(0.0)

  override def fromInt(n: Int): BigDecimal = BigDecimal(n)
  override def fromBigInt(n: BigInt): BigDecimal = BigDecimal(n)
  override def fromDouble(n: Double): BigDecimal = BigDecimal(n)
  override def fromBigDecimal(n: BigDecimal): BigDecimal = n

  def div(a: BigDecimal, b: BigDecimal): BigDecimal = a / b

}

trait BigDecimalInstances {
  implicit final val BigDecimalAlgebra: Order[BigDecimal] & Field[BigDecimal] = new BigDecimalAlgebra
}
