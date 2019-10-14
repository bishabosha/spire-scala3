package spire.syntax

import spire.algebra.{Field, CRing}

object primitives {

  inline def (n: => Int) as [A](given A: CRing[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromInt(n)

  inline def (n: => Double) as [A](given A: Field[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromDouble(n)

}
