package spire
package math

import java.lang.Math

object FloatComplex {
  import FastComplex.{encode}

  final def apply(real: Float, imag: Float): FloatComplex =
    new FloatComplex(encode(real, imag))

  final def apply(real: Double, imag: Double): FloatComplex =
    new FloatComplex(encode(real.toFloat, imag.toFloat))

  final val i: FloatComplex = new FloatComplex(4575657221408423936L)
  final val one: FloatComplex = new FloatComplex(1065353216L)
  final val zero: FloatComplex = new FloatComplex(0L)
}

/**
  * Value class which encodes two floating point values in a Long.
  *
  * We get (basically) unboxed complex numbers using this hack.
  * The underlying implementation lives in the FastComplex object.
  */
class FloatComplex(val u: Long) extends AnyVal {
  override final def toString: String = "(%s+%si)" format (real, imag)

  final def real: Float = FastComplex.real(u)
  final def imag: Float = FastComplex.imag(u)
  final def repr: String = "FloatComplex(%s, %s)" format(real, imag)
  final def conjugate: FloatComplex = new FloatComplex(FastComplex.conjugate(u))
  final def isWhole: Boolean = FastComplex.isWhole(u)
  final def signum: Int = FastComplex.signum(u)
  final def complexSignum: FloatComplex = new FloatComplex(FastComplex.complexSignum(u))
  final def negate: FloatComplex = new FloatComplex(FastComplex.negate(u))

  final def +(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.add(u, b.u))
  final def -(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.subtract(u, b.u))
  final def *(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.multiply(u, b.u))
  final def /(b: FloatComplex): FloatComplex = new FloatComplex(FastComplex.divide(u, b.u))
}


/**
  * FastComplex is an ugly, beautiful hack.
  *
  * The basic idea is to encode two 32-bit Floats into a single 64-bit Long.
  * The lower-32 bits are the "real" Float and the upper-32 are the "imaginary"
  * Float.
  *
  * Since we're overloading the meaning of Long, all the operations have to be
  * defined on the FastComplex object, meaning the syntax for using this is a
  * bit ugly. To add to the ugly beauty of the whole thing I could imagine
  * defining implicit operators on Long like +@, -@, *@, /@, etc.
  *
  * You might wonder why it's even worth doing this. The answer is that when
  * you need to allocate an array of e.g. 10-20 million complex numbers, the GC
  * overhead of using *any* object is HUGE. Since we can't build our own
  * "pass-by-value" types on the JVM we are stuck doing an encoding like this.
  *
  * Here are some profiling numbers for summing an array of complex numbers,
  * timed against a concrete case class implementation using Float (in ms):
  *
  *  size | encoded |  class
  *    1M |     5.1 |    5.8
  *    5M |    28.5 |   91.7
  *   10M |    67.7 |  828.1
  *   20M |   228.0 | 2687.0
  *
  * Not bad, eh?
  */
object FastComplex {
  import java.lang.Math.{atan2, cos, sin}

  // note the superstitious use of @inline and final everywhere

  final def apply(real: Float, imag: Float): Long = encode(real, imag)
  final def apply(real: Double, imag: Double): Long = encode(real.toFloat, imag.toFloat)

  // encode a float as some bits
  @inline final def bits(n: Float): Int = java.lang.Float.floatToIntBits(n)

  // decode some bits into a float
  @inline final def bits(n: Int): Float = java.lang.Float.intBitsToFloat(n)

  // get the real part of the complex number
  @inline final def real(d: Long): Float = bits((d & 0xffffffff).toInt)

  // get the imaginary part of the complex number
  @inline final def imag(d: Long): Float = bits((d >>> 32).toInt)

  // define some handy constants
  final val i: Long = encode(0.0F, 1.0F)
  final val one: Long = encode(1.0F, 0.0F)
  final val zero: Long = encode(0.0F, 0.0F)

  // encode two floats representing a complex number
  @inline final def encode(real: Float, imag: Float): Long =
    (bits(real) & 0xffffffffL) | ((bits(imag) & 0xffffffffL) << 32)

  // decode should be avoided in fast code because it allocates a Tuple2.
  final def decode(d: Long): (Float, Float) = (real(d), imag(d))

  // produces a string representation of the Long/(Float,Float)
  final def toRepr(d: Long): String = "FastComplex(%s -> %s)" format(d, decode(d))

  // get the complex conjugate
  final def conjugate(d: Long): Long = encode(real(d), -imag(d))

  // see if the complex number is a whole value
  final def isWhole(d: Long): Boolean = real(d) % 1.0F == 0.0F && imag(d) % 1.0F == 0.0F

  // get the sign of the complex number
  final def signum(d: Long): Int = real(d) compare 0.0F

  // get the complex sign of the complex number
  final def complexSignum(d: Long): Long = {
    val m = Math.abs(d)
    if (m == 0.0F) zero else divide(d, encode(m, 0.0F))
  }

  // negation
  final def negate(a: Long): Long = encode(-real(a), -imag(a))

  // addition
  final def add(a: Long, b: Long): Long = encode(real(a) + real(b), imag(a) + imag(b))

  // subtraction
  final def subtract(a: Long, b: Long): Long = encode(real(a) - real(b), imag(a) - imag(b))

  // multiplication
  final def multiply(a: Long, b: Long): Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)
    encode(re_a * re_b - im_a * im_b, im_a * re_b + re_a * im_b)
  }

  // division
  final def divide(a: Long, b: Long): Long = {
    val re_a = real(a)
    val im_a = imag(a)
    val re_b = real(b)
    val im_b = imag(b)

    val abs_re_b = Math.abs(re_b)
    val abs_im_b = Math.abs(im_b)

    if (abs_re_b >= abs_im_b) {
      if (abs_re_b == 0.0F) throw new ArithmeticException("/0")
      val ratio = im_b / re_b
      val denom = re_b + im_b * ratio
      encode((re_a + im_a * ratio) / denom, (im_a - re_a * ratio) / denom)

    } else {
      if (abs_im_b == 0.0F) throw new ArithmeticException("/0")
      val ratio = re_b / im_b
      val denom = re_b * ratio + im_b
      encode((re_a * ratio + im_a) / denom, (im_a * ratio - re_a) / denom)
    }
  }

}
