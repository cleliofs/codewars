import CountOccurrences.count
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CountOccurrencesTest extends AnyFlatSpec with Matchers {

  behavior of "CountOccurrences"

  it should "have right occurrences" in {
    assert(count("aba") == Map[Char, Int]('a' -> 2, 'b' -> 1))
    assert(count("") == Map[Char, Int]())
  }

}
