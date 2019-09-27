package spire
package math

import language.implicitConversions

import spire.algebra.{CRig, Order}

object UInt {

  implicit final val algebra: Order[UInt] with CRig[UInt] = UIntAlgebra()

  final def apply(n: Long): UInt = UInt(summon[Numeric[Long]].toLong(n))

  @inline final val MinValue: UInt = UInt(0)
  @inline final val MaxValue: UInt = UInt(-1)
}

class UInt(val signed: Int) extends AnyVal {
  def toByte: Byte = signed.toByte
  def toChar: Char = signed.toChar
  def toShort: Short = signed.toShort
  def toInt: Int = signed
  def toLong: Long = signed & 0xffffffffL
  def toFloat: Float = toLong.toFloat
  def toDouble: Double = toLong.toDouble
  def toBigInt: BigInt = BigInt(toLong)

  def isValidByte: Boolean = toInt == toByte
  def isValidShort: Boolean = toInt == toShort
  def isValidChar: Boolean = toInt == toChar
  def isValidInt: Boolean = signed >= 0
  def isValidLong: Boolean = true

  override def toString: String = toLong.toString

  def == (that: UInt): Boolean = this.signed == that.signed
  def != (that: UInt): Boolean = this.signed != that.signed

  def ===(that: UInt): Boolean = this.signed == that.signed
  def =!=(that: UInt): Boolean = this.signed != that.signed

  def <= (that: UInt): Boolean = this.toLong <= that.toLong
  def < (that: UInt): Boolean = this.toLong < that.toLong
  def >= (that: UInt): Boolean = this.toLong >= that.toLong
  def > (that: UInt): Boolean = this.toLong > that.toLong

  def unary_- : UInt = UInt(-this.signed)

  def + (that: UInt): UInt = UInt(this.signed + that.signed)
  def - (that: UInt): UInt = UInt(this.signed - that.signed)
  def * (that: UInt): UInt = UInt(this.signed * that.signed)
  def / (that: UInt): UInt = UInt(this.toLong / that.toLong)
  def % (that: UInt): UInt = UInt(this.toLong % that.toLong)

  def unary_~ : UInt = UInt(~this.signed)

  def << (shift: Int): UInt = UInt(signed << shift)
  def >> (shift: Int): UInt = UInt(signed >>> shift)
  def >>> (shift: Int): UInt = UInt(signed >>> shift)
  def & (that: UInt): UInt = UInt(this.signed & that.signed)
  def | (that: UInt): UInt = UInt(this.signed | that.signed)
  def ^ (that: UInt): UInt = UInt(this.signed ^ that.signed)
}

final class UIntAlgebra extends Order[UInt] with CRig[UInt] {

  // Order

  def compare(x: UInt, y: UInt): Int = if (x < y) -1 else if (x > y) 1 else 0
  override def eqv(x:UInt, y:UInt): Boolean = x == y
  override def neqv(x:UInt, y:UInt): Boolean = x != y
  override def gt(x: UInt, y: UInt): Boolean = x > y
  override def gteqv(x: UInt, y: UInt): Boolean = x >= y
  override def lt(x: UInt, y: UInt): Boolean = x < y
  override def lteqv(x: UInt, y: UInt): Boolean = x <= y

  // CRig

  def one: UInt = UInt(1)
  def plus(a:UInt, b:UInt): UInt = a + b
  override def times(a:UInt, b:UInt): UInt = a * b
  def zero: UInt = UInt(0)

}
