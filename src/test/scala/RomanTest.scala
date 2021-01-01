import Roman.encode
import org.scalatest.funsuite.AnyFunSuite

class RomanTest extends AnyFunSuite {

  test("Samples") {
//    assert(encode(1) === "I")
//    assert(encode(3) === "III")
//    assert(encode(4) === "IV")
//    assert(encode(6) === "VI")
//    assert(encode(14) === "XIV")
//    assert(encode(21) === "XXI")
//    assert(encode(89) === "LXXXIX")
    assert(encode(91) === "XCI")
    assert(encode(984) === "CMLXXXIV")
    assert(encode(1000) === "M")
    assert(encode(1666) === "MDCLXVI")
    assert(encode(1889) === "MDCCCLXXXIX")
    assert(encode(1989) === "MCMLXXXIX")
    assert(encode(2008) === "MMVIII")
  }

}
