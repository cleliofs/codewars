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


  // "aabbbaaaabba" => 2a3b4a2ba => Map(0 ->('a', 2), 1->('b', 3), 2->('a', 4), 3->('b', 2), 4->('a', 1))

  // (Map(0 -> ('a', 1)), 'a', 0) and 'a' => (Map(0 -> ('a', 2)), 'a', 0) and 'a'
  // (Map(0 -> ('a', 2)), 'a', 0) and 'b' => (Map(0 -> ('a', 2), 1 -> ('b', 1)), 'b', 1) and 'b'
  // (Map(0 -> ('a', 2), 1 -> ('b', 1)), 'b', 1) and 'b' and 'b' => (Map(0 -> ('a', 2), 1 -> ('b', 2)), 'b', 1) and 'b'
  // (Map(0 -> ('a', 2), 1 -> ('b', 2)), 'b', 1) and 'b' => (Map(0 -> ('a', 2), 1 -> ('b', 3)), 'b', 1) and 'b'
  // (Map(0 -> ('a', 2), 1 -> ('b', 3)), 'b', 1) and 'c' => (Map(0 -> ('a', 2), 1 -> ('b', 3), 2 -> ('c', 1)), 'b', 2) and 'c'
  // ...
  // Map(0 ->('a', 2), 1->('b', 3), 2->('a', 4), 3->('b', 2), 4->('a', 1))
}
