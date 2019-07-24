package spire
package std

import language.implicitConversions

import java.math.BigInteger

import spire.algebra.{CRing, Eq, Order}

final class BigIntegerAlgebra extends Order[BigInteger] with CRing[BigInteger] {
  // Order

  def compare(x: BigInteger, y: BigInteger): Int = x compareTo y
  override def eqv(x:BigInteger, y:BigInteger): Boolean = x equals y
  override def neqv(x:BigInteger, y:BigInteger): Boolean = !(x equals y)
  override def gt(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) > 0
  override def gteqv(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) >= 0
  override def lt(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) < 0
  override def lteqv(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) <= 0

  // CRing

  override def minus(a: BigInteger, b: BigInteger): BigInteger = a subtract b
  def negate(a: BigInteger): BigInteger = a.negate
  def one: BigInteger = BigInteger.ONE
  def plus(a: BigInteger, b: BigInteger): BigInteger = a add b
  override def times(a: BigInteger, b: BigInteger): BigInteger = a multiply b
  def zero: BigInteger = BigInteger.ZERO

  override def fromInt(n: Int): BigInteger = BigInteger.valueOf(n)
}

trait BigIntegerInstances {
  implicit final val BigIntegerAlgebra: BigIntegerAlgebra = new BigIntegerAlgebra
}
