package spire

import spire.algebra.Order
import delegate spire.implicits.{for Order[Int]}

object Example {

  def main(args: Array[String]): Unit = {
    val array = Array(2, 3, 1, 4)
    spire.math.Sorting.sort(array)
    println(array.mkString("(",",",")"))
  }

  val msg = "I was compiled by dotty :)"

}
