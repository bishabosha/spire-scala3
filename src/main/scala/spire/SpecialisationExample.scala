package spire

import spire.algebra.strawman.{ CRig, Semigroup }
import given CRig.{ _: CRig[_] }, Semigroup.{ _: Semigroup[_] }
import spire.math.strawman.Box
import given spire.algebra.strawman.std.{ _: CRig[Int] | CRig[Long] | CRig[Unit] }, Box.{ _: CRig[Box[_]] }

object SpecialisationExample {

  inline def add[T: Semigroup](x: T, y: T): T = x + y
  inline def two[T] given (T: CRig[T]): T.Param = add(T.one, T.one)

  val x = two[Box[Int]]
  val y = two[Box[Long]]
  val z = two[Box[Unit]]
}
