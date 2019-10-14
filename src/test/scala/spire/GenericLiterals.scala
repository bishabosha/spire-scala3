package spire

import spire.syntax.literals.given
import spire.algebra.{Field, CRing}

object GenericLiterals {

  // Field

  def zeroF[A: Field]: A = 0.0                // summon[Field[A]].zero
  def oneF[A: Field]: A  = 1                  // summon[Field[A]].one
  def twoF[A: Field]: A  = 2                  // summon[Field[A]].fromInt(2)
  def three[A: Field]: A = 3.0                // summon[Field[A]].fromDouble(3.0)
  def bigF[A: Field]: A  = 37237283824892382  // summon[Field[A]].fromBigInt(BigInt(Array[Byte](0,-124,75,28,-62,-56,121,-34)))
  def huge[A: Field]: A  = 1e1000             // summon[Field[A]].fromBigDecimal(BigDecimal("1E+1000"))

  // CRing

  def zero[A: CRing]: A  = 0                  // summon[CRing[A]].zero
  def one[A: CRing]: A   = 1                  // summon[CRing[A]].one
  def two[A: CRing]: A   = 2                  // summon[CRing[A]].fromInt(2)
  def hexI[A: CRing]: A  = 0xCAFE             // summon[CRing[A]].fromInt(51966)
  def hexB[A: CRing]: A  = 0xCAFEBABE000000F  // summon[CRing[A]].fromBigInt(BigInt(Array[Byte](12,-81,-21,-85,-32,0,0,15)))
  def big[A: CRing]: A   = 37237283824892382  // summon[CRing[A]].fromBigInt(BigInt(Array[Byte](0,-124,75,28,-62,-56,121,-34)))
}
