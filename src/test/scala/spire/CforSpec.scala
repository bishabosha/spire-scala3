package spire

import language.implicitConversions

import org.scalacheck.Properties
import org.scalacheck._
import Prop._
import Gen.{posNum, choose}
import syntax.cfor._
import collection.mutable.ListBuffer

object CforSpec extends Properties("cfor") {

  def fillList[N](f: ListBuffer[N] => Unit): List[N] = {
    var b = ListBuffer.empty[N]
    f(b)
    b.toList
  }
  
  val range = choose(1,10)
  val rangeL = choose(1L,10L)

  property("fillList with cfor") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cfor(1)(_ <= n, _ + 1)(b += _)
    }

    List.tabulate(n)(_ + 1) =? fill(n)
  }

  property("fillList with cforRange using to") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 to n)(b += _)
    }

    List.range(0, n + 1) =? fill(n)
  }

  property("fillList with cforRangeL using to") = forAll(posNum[Long]) { n =>

    def fill(n: Long): List[Long] = fillList { b =>
      cforRangeL(0L to n)(b += _)
    }

    List.range(0L, n + 1L) =? fill(n)
  }

  property("fillList with cforRange using to, by (positive)") = forAll(posNum[Int], range) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 to n by step)(b += _)
    }

    List.range(0, n + 1, step) =? fill(n, step)
  }

  property("fillList with cforRangeL using to, by (positive)") = forAll(posNum[Long], rangeL) { (n, step) =>

    def fill(n: Long, step: Long): List[Long] = fillList { b =>
      cforRangeL(0L to n by step)(b += _)
    }

    List.range(0L, n + 1L, step) =? fill(n, step)
  }

  property("fillList with cforRange using to, by (negative)") = forAll(posNum[Int], range) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 to n by step)(b += _)
    }

    List.range(0, n - 1, -step) =? fill(n, -step)
  }

  property("fillList with cforRangeL using to, by (negative)") = forAll(posNum[Long], rangeL) { (n, step) =>

    def fill(n: Long, step: Long): List[Long] = fillList { b =>
      cforRangeL(0L to n by step)(b += _)
    }

    List.range(0L, n - 1, -step) =? fill(n, -step)
  }

  property("fillList with cforRange using until, by (positive)") = forAll(posNum[Int], range) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 until n by step)(b += _)
    }

    List.range(0, n, step) =? fill(n, step)
  }

  property("fillList with cforRangeL using until, by (positive)") = forAll(posNum[Long], rangeL) { (n, step) =>

    def fill(n: Long, step: Long): List[Long] = fillList { b =>
      cforRangeL(0L until n by step)(b += _)
    }

    List.range(0L, n, step) =? fill(n, step)
  }

  property("fillList with cforRange using until, by (negative)") = forAll(posNum[Int], range) { (n, step) =>

    def fill(n: Int, step: Int): List[Int] = fillList { b =>
      cforRange(0 until n by step)(b += _)
    }

    List.range(0, n, -step) =? fill(n, -step)
  }

  property("fillList with cforRangeL using until, by (negative)") = forAll(posNum[Long], rangeL) { (n, step) =>

    def fill(n: Long, step: Long): List[Long] = fillList { b =>
      cforRangeL(0L until n by step)(b += _)
    }

    List.range(0L, n, -step) =? fill(n, -step)
  }

  property("fillList with cforRange using until") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 until n)(b += _)
    }

    List.range(0, n) =? fill(n)
  }

  property("fillList with cforRangeL using until") = forAll(posNum[Long]) { n =>

    def fill(n: Long): List[Long] = fillList { b =>
      cforRangeL(0L until n)(b += _)
    }

    List.range(0L, n) =? fill(n)
  }
}
