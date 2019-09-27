package spire.algebra.strawman

import spire.{ Tag, erasedTag }

/**Semigroup as a match type, allowing for use site specialisation of the type parameter.
 * - does cause issues with unification of the type parameter T needing casts, until another way is found.
 */
type Semigroup[T] <: Semigroups.Semigroup[?] = T match {
  case Int  => Semigroups.Specialised.IntSemigroup
  case Long => Semigroups.Specialised.LongSemigroup
  case _    => Semigroups.Semigroup[T]
}

object Semigroup {
  inline given [T](given T: Semigroup[T]): Semigroup[T.Param] = T.asInstanceOf
}

object Semigroups {

  trait Semigroup[A] {

    final type Param = A

    inline def (x: T) + [T] (y: T): T = inline erasedTag[T] match { case _: Tag[A] =>
      x plus y
    }

    def (x: A) plus (y: A): A

  }

  object Specialised {
    trait IntSemigroup extends Semigroup[Int] {
      def (x: Int) plus (y: Int): Int
    }
    trait LongSemigroup extends Semigroup[Long] {
      def (x: Long) plus (y: Long): Long
    }
  }
}
