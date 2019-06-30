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

  property("fillList with cfor") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cfor(1)(_ <= n, _ + 1)(b += _)
    }

    List.tabulate(n)(_ + 1) =? fill(n)
  }

  property("fillList with peek using to") = forAll(posNum[Int]) { n =>
    List.range(0, n + 1) =? fillList { b => 0 to n peek (b += _) }
  }

  property("fillList with peek[Long] using to") = forAll(posNum[Long]) { n =>
    List.range(0L, n + 1L) =? fillList { b => 0L to n peek (b += _) }
  }

  property("fillList with cforRange using to, by (positive)") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 to n by 1)(b += _)
    }

    List.range(0, n + 1, 1) =? fill(n)
  }

  property("fillList with cforRange[Long] using to, by (positive)") = forAll(posNum[Long]) { n =>

    def fill(n: Long): List[Long] = fillList { b =>
      cforRange(0L to n by 1)(b += _)
    }

    List.range(0L, n + 1L, 1L) =? fill(n)
  }

  property("fillList with cforRange using to, by (negative)") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 to -n by -1)(b += _)
    }

    List.range(0, -n - 1, -1) =? fill(n)
  }

  property("fillList with cforRange[Long] using to, by (negative)") = forAll(posNum[Long]) { n =>

    def fill(n: Long): List[Long] = fillList { b =>
      cforRange(0L to -n by -1)(b += _)
    }

    List.range(0L, -n - 1, -1L) =? fill(n)
  }

  property("fillList with cforRange using until, by (positive)") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 until n by 1)(b += _)
    }

    List.range(0, n, 1) =? fill(n)
  }

  property("fillList with cforRange[Long] using until, by (positive)") = forAll(posNum[Long]) { n =>

    def fill(n: Long): List[Long] = fillList { b =>
      cforRange(0L until n by 1)(b += _)
    }

    List.range(0L, n, 1L) =? fill(n)
  }

  property("fillList with cforRange using until, by (negative)") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 until -n by -1)(b += _)
    }

    List.range(0, -n, -1) =? fill(n)
  }

  property("fillList with cforRange[Long] using until, by (negative)") = forAll(posNum[Long]) { n =>

    def fill(n: Long): List[Long] = fillList { b =>
      cforRange(0L until -n by -1)(b += _)
    }

    List.range(0L, -n, -1L) =? fill(n)
  }

  property("fillList with cforRange using until") = forAll(posNum[Int]) { n =>

    def fill(n: Int): List[Int] = fillList { b =>
      cforRange(0 until n)(b += _)
    }

    List.range(0, n) =? fill(n)
  }

  property("fillList with cforRange[Long] using until") = forAll(posNum[Long]) { n =>

    def fill(n: Long): List[Long] = fillList { b =>
      cforRange(0L until n)(b += _)
    }

    List.range(0L, n) =? fill(n)
  }
}
