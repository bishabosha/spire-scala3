package spire.algebra.strawman

import spire.{ Tag, tag }

/**CRig as a match type, allowing for use site specialisation of the type parameter.
 * - does cause issues with unification of the type parameter T needing casts, until another way is found.
 */
type CRig[T] <: CRigs.CRig[_] = T match {
  case Int  => CRigs.Specialised.IntCRig
  case Long => CRigs.Specialised.LongCRig
  case _    => CRigs.CRig[T]
}

object CRigs {

  trait CRig[A] extends Semigroups.Semigroup[A] {

    inline def (x: T) * [T] (y: T): T = inline tag[T] match { case _: Tag[A] =>
      x times y
    }

    def zero: A
    def one: A
    def (x: A) times (y: A): A

  }

  object Specialised {
    trait IntCRig extends CRig[Int] {
      def zero: Int
      def one: Int
      def (x: Int) times (y: Int): Int
    }
    trait LongCRig extends CRig[Long] {
      def zero: Long
      def one: Long
      def (x: Long) times (y: Long): Long
    }
  }
}