import DigPowTest._
import org.scalatest.Assertions._
import org.scalatest.flatspec.AnyFlatSpec

import java.util.{Arrays, Collections}

class DigPowTest extends AnyFlatSpec {

  behavior of "DigPow"

  it should "pass basic tests" in {
    testing(89, 1, 1)
    testing(92, 1, -1)
    testing(46288, 3, 51)
    testing(114, 3, 9)
    testing(46288, 5, -1)
    testing(135, 1, 1)
    testing(175, 1, 1)
    testing(518, 1, 1)
    testing(598, 1, 1)
    testing(1306, 1, 1)
    testing(2427, 1, 1)
    testing(2646798, 1, 1)
    testing(3456789, 1, -1)
    testing(3456789, 5, -1)
    testing(198, 1, 3)
    testing(249, 1, 3)
    testing(1377, 1, 2)
    testing(1676, 1, 1)
    testing(695, 2, 2)
    testing(1878, 2, 19)
    testing(7388, 2, 5)
    testing(47016, 2, 1)
    testing(542186, 2, 1)
    testing(261, 3, 5)
    testing(1385, 3, 35)
    testing(2697, 3, 66)
    testing(6376, 3, 10)
    testing(6714, 3, 1)
    testing(63760, 3, 1)
    testing(63761, 3, 1)
    testing(132921, 3, 4)
    testing(10383, 6, 12933)
  }
  it should "pass random tests" in {
    wtests1()
  }
}

object DigPowTest {

  private def testing(n: Int, p: Int, expect: Int): Unit = {
    println("Testing: " + n + ", " + p)
    val actual: Int = DigPow.digPow(n, p)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("*")
    assertResult(expect){actual}
  }
//............................................
  private def digPow47(n: Int, p: Int): Int = {
    var s: Int = 0
    val nstr: String = n.toString
    for (i <- 0 until nstr.length) {
      s += Math.pow((nstr.charAt(i) - '0').toInt, p + i).toInt
    }
    if (s % n == 0) s / n else -1
  }
  var a = Array((89, 1, 1), (92, 1, -1), (46288, 3, 51), (114, 3, 9), (46288, 5, -1), (135, 1, 1), (175, 1, 1),
    (518, 1, 1), (598, 1, 1), (1306, 1, 1), (2427, 1, 1), (2646798, 1, 1), (3456789, 1, -1), (3456789, 5, -1),
    (198, 1, 3), (249, 1, 3), (1377, 1, 2), (1676, 1, 1), (695, 2, 2), (1878, 2, 19),
    (7388, 2, 5), (47016, 2, 1), (542186, 2, 1), (261, 3, 5), (1385, 3, 35), (6376, 3, 10),
    (6714, 3, 1), (63760, 3, 1), (63761, 3, 1), (132921, 3, 4), (10383, 6, 12933)) // 31

  private def shuffleT(a: Array[(Int, Int, Int)]): Array[(Int, Int, Int)] = {
      Collections.shuffle(Arrays.asList(a: _*))
      a
  }
//............................................
  private def randint(min: Int, max: Int): Int =
    (min + Math.random() * ((max - min) + 1)).toInt
  
  def wtests1(): Unit = {
    for (i <- 0.until(20)) {
      shuffleT(a)
      val (n, p, sol) = a(0)
      testing(n, p, sol)
    }
  }
}