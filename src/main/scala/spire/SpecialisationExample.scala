package spire

import spire.algebra.strawman.{ CRig, Semigroup }
import CRig.{ given CRig[?] }, Semigroup.{ given Semigroup[?] }
import spire.math.strawman.Box
import spire.algebra.strawman.std.{ given CRig[Int] | CRig[Long] | CRig[Unit] }, Box.{ given CRig[Box[?]] }

object SpecialisationExample {

  inline def add[T: Semigroup](x: T, y: T): T = x + y
  inline def two[T](given T: CRig[T]): T.Param = add(T.one, T.one)

  val x = two[Box[Int]]
  val y = two[Box[Long]]
  val z = two[Box[Unit]]
}
