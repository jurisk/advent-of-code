import scala.io.Source

object Advent01 extends App:
  val list: List[Int] = Source.fromResource("01.txt").getLines().map(_.toInt).toList

  def solve(x: Int): Option[Long] =
    list
      .combinations(x)
      .find { _.sum == 2020 }
      .map { _.product }
  
  def f(x: Int) =
    println(solve(x).getOrElse("Failed"))
  
  List(2, 3) foreach f
