package spire.algebra.strawman

import spire.{ Tag, erasedTag }

/**CRig as a match type, allowing for use site specialisation of the type parameter.
 * - does cause issues with unification of the type parameter T needing casts, until another way is found.
 */
type CRig[T] <: CRigs.CRig[?] = T match {
  case Int  => CRigs.Specialised.IntCRig
  case Long => CRigs.Specialised.LongCRig
  case _    => CRigs.CRig[T]
}

object CRig {
  inline given [T](given T: CRig[T]): CRig[T.Param] = T.asInstanceOf
}

object CRigs {

  trait CRig[A] extends Semigroups.Semigroup[A] {

    inline def (x: T) * [T] (y: T): T = inline erasedTag[T] match { case _: Tag[A] =>
      x times y
    }

    def zero: A
    def one: A
    def (x: A) times (y: A): A

  }

  object Specialised {
    trait IntCRig extends CRig[Int], Semigroup[Int] {
      def zero: Int
      def one: Int
      def (x: Int) times (y: Int): Int
    }
    trait LongCRig extends CRig[Long], Semigroup[Long] {
      def zero: Long
      def one: Long
      def (x: Long) times (y: Long): Long
    }
  }
}
