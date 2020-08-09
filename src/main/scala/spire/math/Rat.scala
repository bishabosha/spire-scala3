package spire.math

import spire.algebra._

final class Rat(val num: BigInt, val den: BigInt) extends Serializable { lhs =>

  override def toString: String =
    if (den == 1) s"$num" else s"$num/$den"

  override def equals(that: Any): Boolean =
    that match {
      case r: Rat => num == r.num && den == r.den
      case _ => false
    }

  override def hashCode(): Int = (num, den).##

  def isZero: Boolean = num == 0

  def isOne: Boolean = num == 1 && den == 1

  def compare(rhs: Rat): Int =
    (lhs.num * rhs.den) `compare` (rhs.num * lhs.den)

  def abs(): Rat = Rat(num.abs, den)

  def signum(): Int = num.signum

  def +(rhs: Rat): Rat =
    Rat((lhs.num * rhs.den) + (rhs.num * lhs.den), (lhs.den * rhs.den))

  def unary_- : Rat =
    Rat(-lhs.num, lhs.den)

  def *(rhs: Rat): Rat =
    Rat(lhs.num * rhs.num, lhs.den * rhs.den)

  def /~(rhs: Rat) = lhs / rhs

  def %(rhs: Rat) = Rat.zero

  def reciprocal: Rat =
    if (num == 0) throw new ArithmeticException("/0") else Rat(den, num)

  def /(rhs: Rat): Rat =
    lhs * rhs.reciprocal

  def **(k: Int): Rat =
    Rat(num.pow(k), den.pow(k))

  def toDouble: Double = num.toDouble / den.toDouble

  def toInt: Int = toDouble.toInt

  def isWhole: Boolean = den == 1

  def ceil: Rat =
    if (num >= 0) Rat((num + den - 1) / den, 1)
    else Rat(num / den, 1)

  def floor: Rat =
    if (num >= 0) Rat(num / den, 1)
    else Rat((num - den + 1) / den, 1)

  def round: Rat =
    if (num >= 0) Rat((num + (den / 2)) / den, 1)
    else Rat((num - (den / 2)) / den, 1)

}

object Rat {

  val zero: Rat = Rat(0)
  val one: Rat  = Rat(1)

  def apply(n: BigInt): Rat =
    Rat(n, 1)

  def apply(num: BigInt, den: BigInt): Rat =
    if (den == 0) throw new ArithmeticException("/0")
    else if (den < 0) apply(-num, -den)
    else if (num == 0) new Rat(0, 1)
    else {
      val g = num `gcd` den
      new Rat(num / g, den / g)
    }

  def unapply(r: Rat): (BigInt, BigInt) = (r.num, r.den)

  given as Order[Rat], Field[Rat] {

    def compare(x: Rat, y: Rat): Int = x `compare` y

    val zero: Rat = Rat.zero
    val one: Rat = Rat.one

    def plus(a: Rat, b: Rat): Rat = a + b
    def negate(a: Rat): Rat = -a
    def times(a: Rat, b: Rat): Rat = a * b
    override def reciprocal(a: Rat): Rat = a.reciprocal
    def div(a: Rat, b: Rat): Rat = a / b

    override def fromInt(n: Int): Rat = Rat(n)
    override def fromBigInt(n: BigInt): Rat = Rat(n)

  }

}
