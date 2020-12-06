import scala.io.Source

object Advent06 extends App:
  opaque type Form = Set[Char]
  opaque type Group = Set[Form]
  
  val groups: List[Group] = Source
    .fromResource("06.txt")
    .getLines()
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toSet).toSet)
    .toList
  
  val results = List[Group => Int](
    _.flatten.size,
    _.reduce { _.intersect(_) }.size,
  ).map { f => groups.map(f).sum }

  results foreach println
