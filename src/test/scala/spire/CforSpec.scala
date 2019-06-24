package spire

import language.implicitConversions

import org.scalacheck.Properties
import org.scalacheck._
import Prop.forAll
import Gen.{posNum, choose}
import syntax.cfor._
import collection.mutable.ListBuffer

object CforSpec extends Properties("cfor") {

  def fillList(f: ListBuffer[Int] => Unit): List[Int] = {
    var b = ListBuffer.empty[Int]
    f(b)
    b.toList
  }

  property("fillList with cfor") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cfor(1)(_ <= n, _ + 1)(b += _)
    }

    List.tabulate(n)(_ + 1) == fill(n)
  }

  val range = choose(1,10)

  property("fillList with cforRange using to") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 to n)(b += _)
    }

    List.range(0, n + 1) == fill(n)
  }

  property("fillList with cforRange using to, by (positive)") = forAll(posNum[Int], choose(1,100)) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 to n by step)(b += _)
    }

    List.range(0, n + 1, step) == fill(n, step)
  }

  property("fillList with cforRange using to, by (negative)") = forAll(posNum[Int], choose(1,100)) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 to n by step)(b += _)
    }

    List.range(0, n - 1, -step) == fill(n, -step)
  }

  property("fillList with cforRange using until, by (positive)") = forAll(posNum[Int], choose(1,100)) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 until n by step)(b += _)
    }

    List.range(0, n, step) == fill(n, step)
  }

  property("fillList with cforRange using until, by (negative)") = forAll(posNum[Int], choose(1,100)) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 until n by step)(b += _)
    }

    List.range(0, n, -step) == fill(n, -step)
  }

  property("fillList with cforRange using until") = forAll(posNum[Int]) { n: Int =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 until n)(b += _)
    }

    List.range(0, n) == fill(n)
  }
}
