import Roman.encode
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class RomanTest extends AnyFlatSpec with Matchers {

  behavior of "Roman"

  it should "pass for some samples" in {
    assert(encode(1) === "I")
    assert(encode(3) === "III")
    assert(encode(4) === "IV")
    assert(encode(6) === "VI")
    assert(encode(14) === "XIV")
    assert(encode(21) === "XXI")
    assert(encode(89) === "LXXXIX")
    assert(encode(91) === "XCI")
    assert(encode(984) === "CMLXXXIV")
    assert(encode(1000) === "M")
    assert(encode(1666) === "MDCLXVI")
    assert(encode(1889) === "MDCCCLXXXIX")
    assert(encode(1989) === "MCMLXXXIX")
    assert(encode(2008) === "MMVIII")
  }

  it should "pass for more samples" in {
    List(
      List(
        (1, "I"),
        (3, "III"),
        (4, "IV"),
        (6, "VI"),
        (14, "XIV"),
        (21, "XXI"),
        (89, "LXXXIX"),
        (91, "XCI"),
        (984, "CMLXXXIV"),
        (1000, "M"),
        (1666, "MDCLXVI"),
        (1889, "MDCCCLXXXIX"),
        (1989, "MCMLXXXIX"),
        (2008, "MMVIII")
      ),
      (0 to 50).map { _ =>
        val input = 1 + Random.nextInt(2500)
        (input, encode(input))
      }
    )
    .flatten
    .distinct
    .foreach {
      case (input, expected) =>
        encode(input) shouldBe expected
    }

  }

}
