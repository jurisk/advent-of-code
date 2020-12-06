import scala.io.Source

object Advent06 extends App:
  opaque type Answer = Char
  opaque type Form = Set[Answer]
  opaque type Group = Set[Form]
  
  val groups: List[Group] = Source
    .fromResource("06.txt")
    .getLines()
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toSet).toSet)
    .toList
  
  type MergeFunction[T] = (Set[T], Set[T]) => Set[T]
  
  val results = List[MergeFunction[Answer]](
    _.union(_),
    _.intersect(_),
  ).map { f => groups.map(_.reduce(f).size).sum }

  results foreach println
