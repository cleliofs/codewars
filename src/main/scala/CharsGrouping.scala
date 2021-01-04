import scala.collection.immutable.Seq
import scala.collection.mutable

object CharsGrouping extends App {

  def groupChars(s: String): String = {
    def mkStringResults(m: Seq[(Char, Int)]): String = {
      m.map( t => s"${if (t._2 > 1) t._2 else ""}${t._1}")
    }.mkString

    val groupedCharsSeq = s.tail.foldLeft[(mutable.Seq[(Char, Int)], Char, Int)]((mutable.Seq((s.head, 1)), s.head, 0)) { (counter, c) =>
      val (currentSeq, currentChar, currentPosition) = counter
      if (c == currentChar) {
        currentSeq.update(currentPosition, (currentSeq(currentPosition)._1, currentSeq(currentPosition)._2 + 1))
        (currentSeq, c, currentPosition)
      } else {
        val newPosition = currentPosition+1
        (currentSeq ++ Seq((c, 1)), c, newPosition)
      }
    }

    mkStringResults(groupedCharsSeq._1.toSeq)
  }


  // "aabbbaaaabba" => 2a3b4a2ba => List(('a', 2), ('b', 3), ('a', 4), ('b', 2), ('a', 1))

  // (Map(0 -> ('a', 1)), 'a', 0) and 'a' => (Map(0 -> ('a', 2)), 'a', 0) and 'a'
  // (Map(0 -> ('a', 2)), 'a', 0) and 'b' => (Map(0 -> ('a', 2), 1 -> ('b', 1)), 'b', 1) and 'b'
  // (Map(0 -> ('a', 2), 1 -> ('b', 1)), 'b', 1) and 'b' and 'b' => (Map(0 -> ('a', 2), 1 -> ('b', 2)), 'b', 1) and 'b'
  // (Map(0 -> ('a', 2), 1 -> ('b', 2)), 'b', 1) and 'b' => (Map(0 -> ('a', 2), 1 -> ('b', 3)), 'b', 1) and 'b'
  // (Map(0 -> ('a', 2), 1 -> ('b', 3)), 'b', 1) and 'c' => (Map(0 -> ('a', 2), 1 -> ('b', 3), 2 -> ('c', 1)), 'b', 2) and 'c'
  // ...
  // Map(0 ->('a', 2), 1->('b', 3), 2->('a', 4), 3->('b', 2), 4->('a', 1))
}
