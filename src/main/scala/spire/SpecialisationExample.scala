package spire

import spire.algebra.strawman.{ CRig, Semigroup }
import spire.math.strawman.Box
import given spire.algebra.strawman.std.{ for CRig[Int], CRig[Long], CRig[Unit] }, Box.{ for CRig[Box[_]] }

object SpecialisationExample {

  inline def add[T: Semigroup](x: T, y: T): T = x + y
  inline def two[T] given (T: CRig[T]): T.Param = T.one + T.one

  val x = two[Box[Int]]
  val y = two[Box[Long]]
  val z = two[Box[Unit]]

  val add2 = add(Box(1), Box(1))
  val add3 = add(Box(2L), Box(1L))
}
