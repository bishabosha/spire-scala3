package spire.syntax

import spire.algebra.{Field, CRing}

object primitives:

  extension [A](n: Int) inline def as(using A: CRing[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromInt(n)

  extension [A](n: Double) inline def as(using A: Field[A]): A = inline n match
    case 0 => A.zero
    case 1 => A.one
    case n => A.fromDouble(n)

end primitives
