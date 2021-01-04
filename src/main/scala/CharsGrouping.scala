import scala.collection.immutable.Seq

object CharsGrouping extends App {

  def groupChars(s: String): String = {
    def mkStringResults(m: Seq[(Char, Int)]): String = {
      m.map( t => s"${if (t._2 > 1) t._2 else ""}${t._1}")
    }.mkString

    val groupedCharsMap = s.tail.foldLeft[(Map[Int, (Char, Int)], Char, Int)]((Map(0 -> (s.head, 1)), s.head, 0)) { (counter, c) =>
      val (currentMap, currentChar, currentPosition) = counter
      if (c == currentChar) {
        (currentMap + (currentPosition -> (c, currentMap(currentPosition)._2 + 1)), c, currentPosition)
      } else {
        val newPosition = currentPosition+1
        (currentMap + (newPosition -> (c, 1)), c, newPosition)
      }
    }
    mkStringResults(groupedCharsMap._1.toSeq.sortBy(_._1).map(_._2))
  }


  // "aabbbaaaabba")) // abcb4aba

  // Map(1 -> ('a', 2), ('b', 3), ('a', 4), ('b', 2), ('a', 1))

  // (Seq(('a', 1)), 'a', 0)
  // (Seq(('a', 2)), 'a', 0)
  // (Seq(('a', 2)), 'b', 0)
  // Seq(('a', 2), ('b', 1))
  // Seq(('a', 2), ('b', 2))
  // Seq(('a', 2), ('b', 3))
  // Seq(('a', 2), ('b', 3), ('a', 1))
  //
}
