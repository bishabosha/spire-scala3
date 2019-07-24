package spire

import spire.algebra.Order
import given spire.implicits.{for Order[Int]}

object Example extends App {
  val array = Array(2, 3, 1, 4)
  spire.math.Sorting.sort(array)
  println(array.mkString("(",",",")"))
}
