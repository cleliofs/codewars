/*
Create a function taking a positive integer as its parameter and returning a string containing the Roman Numeral representation of that integer.

Modern Roman numerals are written by expressing each digit separately starting with the left most digit and skipping any digit with a value of zero.

However, for the numbers 4 and 9, subtraction is used instead of addition, and the smaller number is written in front of the greater number: e.g.
14 is written as XIV, i.e. 10 + 5 − 1, and 199 is expressed as CXCIX i.e. 100 + 100 − 10 + 10 − 1.

In Roman numerals 1990 is rendered: 1000=M, 900=CM, 90=XC; resulting in MCMXC. 2008 is written as 2000=MM, 8=VIII; or MMVIII. 1666 uses each Roman
symbol in descending order: MDCLXVI.

Example:

Roman.encode(1000) // should return "M"
Help:

Symbol    Value
I          1
V          5
X          10
L          50
C          100
D          500
M          1,000

Remember that there can't be more than 3 identical symbols in a row.

More about roman numerals - http://en.wikipedia.org/wiki/Roman_numerals
*/

import scala.annotation.tailrec
import scala.collection.Seq

object Roman extends App {

  def encode(arabic: Int): String = {

    val symbols = Map(
      (1 -> 'I'),
      (5 -> 'V'),
      (10 -> 'X'),
      (50 -> 'L'),
      (100 -> 'C'),
      (500 -> 'D'),
      (1000 -> 'M')
    )

    val converterMatrix = Map(
      (0 -> Array("", "", "", "")),
      (1 -> Array("M", "C", "X", "I")),
      (2 -> Array("MM", "CC", "XX", "II")),
      (3 -> Array("MM", "CCC", "XXX", "III")),
      (4 -> Array("", "CD", "XL", "IV")),
      (5 -> Array("", "D", "L", "V")),
      (6 -> Array("", "DC", "LX", "VI")),
      (7 -> Array("", "DCC", "LXX", "VII")),
      (8 -> Array("", "DCCC", "LXXX", "VIII")),
      (9 -> Array("", "CM", "XC", "IX"))
    )

    val matrix = Array(
      Array("", "", "", ""),
      Array("M", "C", "X", "I"),
      Array("MM", "CC", "XX", "II"),
      Array("MM", "CCC", "XXX", "III"),
      Array("", "CD", "XL", "IV"),
      Array("", "D", "L", "V"),
      Array("", "DC", "LX", "VI"),
      Array("", "DCC", "LXX", "VII"),
      Array("", "DCCC", "LXXX", "VIII"),
      Array("", "CM", "XC", "IX")
    )

    @tailrec
    def maxDivisor(number: Int, currentSymbolDivisorIndex: Int): Int = {
      val symbolIndices = symbols.keys.toSeq.sorted
      if (number / symbolIndices(currentSymbolDivisorIndex) == 0)
        symbolIndices(currentSymbolDivisorIndex - 1)
      else
        maxDivisor(number, currentSymbolDivisorIndex + 1)
    }

    def maxDivisorFor4And9(number: Int, currentSymbolDivisorIndex: Int): Int = {
      val symbolIndices = symbols.keys.toSeq.sorted
      if (number / symbolIndices(currentSymbolDivisorIndex) == 0)
        symbolIndices(currentSymbolDivisorIndex)
      else
        maxDivisor(number, currentSymbolDivisorIndex + 1)

    }

    @tailrec
    def composeRomanNumber(number: Int, romanNumber: String = ""): String = {
      def composeWith(symbol: Char, n: Int): String = {
        if (n == 1) symbol.toString
        else if (n == 2) s"$symbol$symbol"
        else s"$symbol$symbol$symbol"
      }

      number match {
        case n if n == 4 => s"${romanNumber}IV"
        case n if n == 9 => s"${romanNumber}IX"
        case n if n <= 3 => s"${romanNumber}${composeWith('I', n)}"
        case n =>
          val d = maxDivisor(number, 0)
          val nTimes = number / d
          val s = symbols(d)
          val romanNumberSoFar = s"$romanNumber${composeWith(s, nTimes)}"
          val reminder = number % d
          composeRomanNumber(reminder, romanNumberSoFar)
      }
    }

//    composeRomanNumber(arabic)

    def convertUsingMatrix(row: Int, col: Int): String = matrix(row)(col)

    if (arabic <= 0 || arabic >= 3999) throw new IllegalArgumentException(s"Invalid: $arabic")

    val arabicStrLeftPadded = f"$arabic%04d"
    arabicStrLeftPadded.zipWithIndex.map { t =>
      val (n, i) = t
      convertUsingMatrix(n.asDigit, i)
    }.mkString

  }
}
