import CharsGrouping.groupChars
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CharsGroupingTest extends AnyFlatSpec with Matchers {

  behavior of "CharsGrouping"

  it should "pass for the following inputs" in {
    groupChars("aabbbaaaabba") shouldBe "2a3b4a2ba"
    groupChars("abbbaaaaba") shouldBe "a3b4aba"
    groupChars("abcbaaaaba") shouldBe "abcb4aba"
    groupChars("abccbaaaaba") shouldBe "ab2cb4aba"
    groupChars("abccbaaaabaa") shouldBe "ab2cb4ab2a"
    groupChars("abccbaaaabaaa") shouldBe "ab2cb4ab3a"
  }

}
