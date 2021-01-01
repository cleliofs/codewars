/*

The main idea is to count all the occurring characters in a string. If you have a string like aba, then the result should be {'a': 2, 'b': 1}.

What if the string is empty? Then the result should be empty object literal, {}.

*/
object CountOccurrences extends App {

  def count(string: String): Map[Char, Int] = string.foldLeft(Map[Char, Int]())((m, c) => m ++ Map(c -> (m.getOrElse(c, 0) + 1)))

}
