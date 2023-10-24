package spire.std

import java.math.BigInteger

import spire.algebra.{CRing, Eq, Order}

trait BigIntegerInstances {

  given Order[BigInteger] with CRing[BigInteger] with {

    // Order

    def compare(x: BigInteger, y: BigInteger): Int = x compareTo y
    override def eqv(x:BigInteger, y:BigInteger): Boolean = x equals y
    override def neqv(x:BigInteger, y:BigInteger): Boolean = !(x equals y)
    override def gt(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) > 0
    override def gteqv(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) >= 0
    override def lt(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) < 0
    override def lteqv(x: BigInteger, y: BigInteger): Boolean = (x compareTo y) <= 0

    // CRing

    override def minus(a: BigInteger, b: BigInteger): BigInteger = (a subtract b).nn
    def negate(a: BigInteger): BigInteger = a.negate.nn
    def one: BigInteger = BigInteger.ONE.nn
    def plus(a: BigInteger, b: BigInteger): BigInteger = (a add b).nn
    override def times(a: BigInteger, b: BigInteger): BigInteger = (a multiply b).nn
    def zero: BigInteger = BigInteger.ZERO.nn

    override def fromInt(n: Int): BigInteger = BigInteger.valueOf(n.toLong).nn
    override def fromBigInt(n: BigInt): BigInteger = n.underlying

  }

}
