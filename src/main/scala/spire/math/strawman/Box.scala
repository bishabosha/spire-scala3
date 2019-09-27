package spire.math.strawman

import spire.Tag
import spire.algebra.strawman.{ CRig, Semigroup }

sealed trait Box[T] {
  def +(y: Box[T]) (given Semigroup[T]): Box[T]
  def *(y: Box[T]) (given CRig[T]): Box[T]
}

object Box {
  import Tag._

  given [T: CRig: Tag]: CRig[Box[T]], Semigroup[Box[T]] {
    val zero = point(summon[CRig[T]].zero.asInstanceOf[T]) // must use runtime tag
    val one  = point(summon[CRig[T]].one.asInstanceOf[T])  // must use runtime tag
    def (x: Box[T]) plus (y: Box[T]): Box[T]  = x + y
    def (x: Box[T]) times (y: Box[T]): Box[T] = x * y
  }

  def apply(i: Int):  Box[Int]  = Box.species(i)
  def apply(l: Long): Box[Long] = Box.species(l)
  def apply[A](a: A): Box[A]    = Box.species(a)

  private def point[A: Tag](a: A): Box[A] = Tag[A] match {
    case IntTag  => Box(a)
    case LongTag => Box(a)
    case _       => Box(a)
  }

  inline private def species[T](t: T) (given tag: => Tag[T]): Box[T] = {

    class BoxT(val x: T) extends Box[T] {

      def + (box: Box[T]) (given Semigroup[T]) : BoxT = BoxT(x + box.asInstanceOf[BoxT].x)
      def * (box: Box[T]) (given CRig[T])      : BoxT = BoxT(x * box.asInstanceOf[BoxT].x)

      override def toString = s"Box${tag.render}($x)"
    }

    BoxT(t)

  }
}
