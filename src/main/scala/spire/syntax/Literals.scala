package spire
package syntax

import spire.algebra.{Field, CRing}

object primitives {

  inline def (n: Int) as [A] given (ev: CRing[A]): A = inline n match {
    case 0 => ev.zero
    case 1 => ev.one
    case n => ev.fromInt(n)
  }

}
