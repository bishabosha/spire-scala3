import org.junit.Test
import org.junit.Assert._

class CforTest {
  import spire.syntax.cfor._
  import collection.mutable.ListBuffer
  import java.util.concurrent.ThreadLocalRandom

  val rnd = ThreadLocalRandom.current

  inline val iter    = 300
  inline val maxN    = 100000
  inline val maxStep = 10

  @Test def fillList(): Unit = {

    def fillListCfor(n: Int): List[Int] = {
      var b = ListBuffer.empty[Int]
      cfor(1)(_ <= n, _ + 1)(b += _)
      b.toList
    }

    for (_ <- 1 to iter) {
      val n = rnd.nextInt(0,maxN)

      val expect = List.tabulate(n)(_ + 1)
      val withCfor = fillListCfor(n)

      assertEquals(expect, withCfor)
    }
  }

  @Test def fillListRangeTo(): Unit = {

    def fillListCfor(n: Int): List[Int] = {
      var b = ListBuffer.empty[Int]
      cforRange(0 to n)(b += _)
      b.toList
    }

    for (_ <- 1 to iter) {
      val n = rnd.nextInt(0,maxN)

      val expect = List.range(0, n + 1)
      val withCfor = fillListCfor(n)

      assertEquals(expect, withCfor)
    }
  }

  @Test def fillListRangeToStep(): Unit = {

    def fillListCfor(n: Int, step: Int): List[Int] = {
      var b = ListBuffer.empty[Int]
      cforRange(0 to n by step)(b += _)
      b.toList
    }

    for (_ <- 1 to iter) {
      val nPos    = rnd.nextInt(0,maxN)
      val nNeg    = rnd.nextInt(0,maxN)
      val stepPos = rnd.nextInt(1,maxStep)
      val stepNeg = -rnd.nextInt(1,maxStep)

      val expectPos   = List.range(0, nPos + 1, stepPos)
      val withCforPos = fillListCfor(nPos, stepPos)

      val expectNeg   = List.range(0, nNeg - 1, stepNeg)
      val withCforNeg = fillListCfor(nNeg, stepNeg)

      assertEquals(expectPos, withCforPos)
      assertEquals(expectNeg, withCforNeg)
    }
  }

  @Test def fillListRangeUntil(): Unit = {

    def fillListCfor(n: Int): List[Int] = {
      var b = ListBuffer.empty[Int]
      cforRange(0 until n)(b += _)
      b.toList
    }

    for (_ <- 1 to iter) {
      val n = rnd.nextInt(0,maxN)

      val expect = List.range(0, n)
      val withCfor = fillListCfor(n)

      assertEquals(expect, withCfor)
    }
  }

  @Test def fillListRangeUntilStep(): Unit = {

    def fillListCfor(n: Int, step: Int): List[Int] = {
      var b = ListBuffer.empty[Int]
      cforRange(0 until n by step)(b += _)
      b.toList
    }

    for (_ <- 1 to iter) {
      val nPos    = rnd.nextInt(0,maxN)
      val nNeg    = rnd.nextInt(0,maxN)
      val stepPos = rnd.nextInt(1,maxStep)
      val stepNeg = -rnd.nextInt(1,maxStep)

      val expectPos   = List.range(0, nPos, stepPos)
      val withCforPos = fillListCfor(nPos, stepPos)

      val expectNeg   = List.range(0, nNeg, stepNeg)
      val withCforNeg = fillListCfor(nNeg, stepNeg)

      assertEquals(expectPos, withCforPos)
      assertEquals(expectNeg, withCforNeg)
    }
  }
}
