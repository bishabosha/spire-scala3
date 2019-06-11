package spire
package std

import spire.algebra.{CRing, Order}

final class IntAlgebra extends Order[Int] with CRing[Int] {

  // Order

  def compare(x: Int, y: Int): Int = if (x < y) -1 else if (x == y) 0 else 1
  override def eqv(x: Int, y: Int): Boolean = x == y
  override def neqv(x: Int, y: Int): Boolean = x != y
  override def gt(x: Int, y: Int): Boolean = x > y
  override def gteqv(x: Int, y: Int): Boolean = x >= y
  override def lt(x: Int, y: Int): Boolean = x < y
  override def lteqv(x: Int, y: Int): Boolean = x <= y

  // CRing
  override def minus(a:Int, b:Int): Int = a - b
  def negate(a:Int): Int = -a
  def one: Int = 1
  def plus(a:Int, b:Int): Int = a + b
  override def times(a:Int, b:Int): Int = a * b
  def zero: Int = 0

  override def fromInt(n: Int): Int = n

}


trait IntInstances {
  implicit final val IntAlgebra: Order[Int] with CRing[Int] = new IntAlgebra
}
