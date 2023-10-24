package spire.std

import spire.algebra.{Field, Order}

trait DoubleInstances {

  given Order[Double] with Field[Double] with {

    // Field
    override def minus(a:Double, b:Double): Double = a - b
    def negate(a:Double): Double = -a
    def one: Double = 1.0
    def plus(a:Double, b:Double): Double = a + b
    override def times(a:Double, b:Double): Double = a * b
    def zero: Double = 0.0

    override def fromInt(n: Int): Double = n.toDouble
    override def fromDouble(n: Double): Double = n

    def div(a:Double, b:Double): Double = a / b

    // Order
    override def eqv(x:Double, y:Double): Boolean = x == y
    override def neqv(x:Double, y:Double): Boolean = x != y
    override def gt(x: Double, y: Double): Boolean = x > y
    override def gteqv(x: Double, y: Double): Boolean = x >= y
    override def lt(x: Double, y: Double): Boolean = x < y
    override def lteqv(x: Double, y: Double): Boolean = x <= y
    def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)

  }

}
