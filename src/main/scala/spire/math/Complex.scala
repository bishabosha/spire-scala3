package spire
package math

import spire.algebra._

import spire.syntax.field.given
import spire.syntax.order.given


object Complex {

  def i[@specialized(Float, Double) T](using T: Field[T]): Complex[T] =
    new Complex(T.zero, T.one)

  def one[@specialized(Float, Double) T](using T: Field[T]): Complex[T] =
    new Complex(T.one, T.zero)

  def zero[@specialized(Float, Double) T](using T: Field[T]): Complex[T] =
    new Complex(T.zero, T.zero)

  def fromInt[@specialized(Float, Double) T](n: Int)(using T: Field[T]): Complex[T] =
    new Complex(T.fromInt(n), T.zero)

  def apply[@specialized(Float, Double) T: Field](real: T): Complex[T] =
    new Complex(real, CRing[T].zero)

  given [A: Field: Order]: Field[Complex[A]] with Eq[Complex[A]] with {
    override def minus(a: Complex[A], b: Complex[A]): Complex[A] = a - b
    def negate(a: Complex[A]): Complex[A] = -a
    def one: Complex[A] = Complex.one
    def plus(a: Complex[A], b: Complex[A]): Complex[A] = a + b
    override def times(a: Complex[A], b: Complex[A]): Complex[A] = a * b
    def zero: Complex[A] = Complex.zero
    override def fromInt(n: Int): Complex[A] = Complex.fromInt[A](n)
    def div(a: Complex[A], b: Complex[A]): Complex[A] = a / b
    def eqv(x: Complex[A], y: Complex[A]): Boolean = x eqv y
    override def neqv(x: Complex[A], y: Complex[A]): Boolean = x neqv y
  }

}

/** Complex numbers. Depending on the underlying scalar T, can represent the Gaussian integers (T = BigInt/SafeLong),
  * the Gaussian rationals (T = Rational) or the complex number field (T: Field).
  *
  * Note that we require T to be at least CRing, a commutative ring, so the implementation below is slightly
  * less general than the Cayley-Dickson construction.
  */
final case class Complex[@specialized(Float, Double) T](real: T, imag: T) extends Serializable { lhs =>

  def absSquare(using r: Field[T]): T = real*real + imag*imag

  def conjugate(using f: Field[T]): Complex[T] = new Complex(real, -imag)

  def asTuple: (T, T) = (real, imag)

  def isZero(using f: Field[T], e: Eq[T]): Boolean = real.isZero && imag.isZero
  def isImaginary(using f: Field[T], e: Eq[T]): Boolean = real.isZero
  def isReal(using f: Field[T], e: Eq[T]): Boolean = imag.isZero

  def eqv(b: Complex[T])(using o: Eq[T]): Boolean = real === b.real && imag === b.imag
  def neqv(b: Complex[T])(using o: Eq[T]): Boolean = real =!= b.real || imag =!= b.imag

  def unary_-(using r: Field[T]): Complex[T] = new Complex(-real, -imag)

  def +(rhs: T)(using r: Field[T]): Complex[T] = new Complex(real + rhs, imag)
  def -(rhs: T)(using r: Field[T]): Complex[T] = new Complex(real - rhs, imag)
  def *(rhs: T)(using r: Field[T]): Complex[T] = new Complex(real * rhs, imag * rhs)
  def /(rhs: T)(using r: Field[T]): Complex[T] = new Complex(real / rhs, imag / rhs)

  def +(b: Complex[T])(using r: Field[T]): Complex[T] =
    new Complex(real + b.real, imag + b.imag)

  def -(b: Complex[T])(using r: Field[T]): Complex[T] =
    new Complex(real - b.real, imag - b.imag)

  def *(b: Complex[T])(using r: Field[T]): Complex[T] =
    new Complex(real * b.real - imag * b.imag, imag * b.real + real * b.imag)

  def /(b: Complex[T])(using f: Field[T], o: Order[T]): Complex[T] = {
    val abs_breal = if (b.real < Field[T].zero) -b.real else b.real
    val abs_bimag = if (b.imag < Field[T].zero) -b.imag else b.imag

    if (abs_breal >= abs_bimag) {
      if (abs_breal === f.zero) throw new Exception("/ by zero")
      val ratio = b.imag / b.real
      val denom = b.real + b.imag * ratio
      new Complex((real + imag * ratio) / denom, (imag - real * ratio) / denom)

    } else {
      if (abs_bimag === f.zero) throw new Exception("/ by zero")
      val ratio = b.real / b.imag
      val denom = b.real * ratio + b.imag
      new Complex((real * ratio + imag) / denom, (imag * ratio - real) /denom)
    }
  }

  override def hashCode: Int = (19 * real.##) + (41 * imag.##) + 97

  // not typesafe, so this is the best we can do :(
  override def equals(that: Any): Boolean = that match {
    case that: Complex[_] => (this.real == that.real) && (this.imag == that.real)
    case _ => false
  }

  def ===(that: Complex[_]): Boolean =
    real == that.real && imag == that.imag

  def =!=(that: Complex[_]): Boolean =
    !(this === that)

  override def toString: String = s"($real + ${imag}i)"
}
