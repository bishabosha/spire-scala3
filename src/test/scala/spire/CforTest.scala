import org.junit.Test
import org.junit.Assert._

class CforTest {
  import spire.syntax.cfor._
  import collection.mutable.ListBuffer
  import java.util.concurrent.ThreadLocalRandom

  @Test def fillList(): Unit = {

    val rnd = ThreadLocalRandom.current

    def fillListCfor(n: Int): List[Int] = {
      var b = ListBuffer.empty[Int]
      cfor(1)(_ <= n, _ + 1)(b += _)
      b.toList
    }

    for (i <- 1 to 100) {
      val n = rnd.nextInt(0,100000)

      val expect = List.tabulate(n)(_ + 1)
      val withCfor = fillListCfor(n)

      assertEquals(expect, withCfor)
    }
  }
}