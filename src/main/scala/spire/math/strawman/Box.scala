package spire.math.strawman

import spire.Tag
import spire.algebra.strawman.{ CRig, Semigroup }

sealed trait Box[T] {
  def +(y: Box[T]) given Semigroup[T]: Box[T]
  def *(y: Box[T]) given CRig[T]: Box[T]
}

object Box {
  import Tag._

  given [T: Semigroup: CRig: Tag] as CRig[Box[T]], Semigroup[Box[T]] {
    val zero = point(the[CRig[T]].zero.asInstanceOf[T])
    val one  = point(the[CRig[T]].one.asInstanceOf[T])
    def (x: Box[T]) plus (y: Box[T]): Box[T]  = x + y
    def (x: Box[T]) times (y: Box[T]): Box[T] = x * y
  }

  def apply(i: Int):  Box[Int]  = Box.species(i, "Int")
  def apply(l: Long): Box[Long] = Box.species(l, "Long")
  def apply[A](a: A): Box[A]    = Box.species(a, "A")

  private def point[A: Tag](f: A): Box[A] = Tag[A] match {
    case IntTag  => Box(f)
    case LongTag => Box(f)
    case t       => Box(f)
  }

  inline private def species[T](t: T, inline kind: String): Box[T] = {

    class BoxT(val x: T) extends Box[T] {

      def + (box: Box[T]) given Semigroup[T]: BoxT = (box: @unchecked) match { case box: BoxT =>
        BoxT(x + box.x)
      }

      def * (box: Box[T]) given CRig[T]: BoxT = (box: @unchecked) match { case box: BoxT =>
        BoxT(x * box.x)
      }

      override def toString = s"Box$kind($x)"
    }

    BoxT(t)
  }
}