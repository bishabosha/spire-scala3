package spire
package std

import spire.algebra.{Order, CRing}
import java.lang.Math

final class LongAlgebra extends Order[Long] with CRing[Long] {

  // CRing

  override def minus(a:Long, b:Long): Long = a - b
  def negate(a:Long): Long = -a
  def one: Long = 1L
  def plus(a:Long, b:Long): Long = a + b
  override def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L

  override def fromInt(n: Int): Long = n

  // Order

  override def eqv(x:Long, y:Long): Boolean = x == y
  override def neqv(x:Long, y:Long): Boolean = x != y
  override def gt(x: Long, y: Long): Boolean = x > y
  override def gteqv(x: Long, y: Long): Boolean = x >= y
  override def lt(x: Long, y: Long): Boolean = x < y
  override def lteqv(x: Long, y: Long): Boolean = x <= y
  def compare(x: Long, y: Long): Int = if (x < y) -1 else if (x == y) 0 else 1

}

trait LongInstances {
  implicit final val LongAlgebra: Order[Long] with CRing[Long] = new LongAlgebra
}
