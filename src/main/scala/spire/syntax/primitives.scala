package spire.syntax

import spire.algebra.{Field, CRing}

object primitives:

  inline def [A](n: => Int) as(using A: CRing[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromInt(n)

  inline def [A](n: => Double) as(using A: Field[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromDouble(n)

end primitives
