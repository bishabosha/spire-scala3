package spire.std

import spire.algebra.{Field, Order}

trait FloatInstances {

  given Order[Float] with Field[Float] with {

    // Order

    def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
    override def eqv(x:Float, y:Float): Boolean = x == y
    override def neqv(x:Float, y:Float): Boolean = x != y
    override def gt(x: Float, y: Float): Boolean = x > y
    override def gteqv(x: Float, y: Float): Boolean = x >= y
    override def lt(x: Float, y: Float): Boolean = x < y
    override def lteqv(x: Float, y: Float): Boolean = x <= y

    // Field
    override def minus(a:Float, b:Float): Float = a - b
    def negate(a:Float): Float = -a
    def one: Float = 1.0F
    def plus(a:Float, b:Float): Float = a + b
    override def times(a:Float, b:Float): Float = a * b
    def zero: Float = 0.0F

    override def fromInt(n: Int): Float = n.toFloat
    override def fromDouble(n: Double): Float = n.toFloat

    def div(a:Float, b:Float): Float = a / b

  }

}
