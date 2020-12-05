import scala.io.Source

object Advent2 extends App:
  final case class Item(
    a: Int,
    b: Int,
    letter: Char,
    password: String,
  ):
    def isValid1: Boolean =
      val count = password.count(_ == letter)
      (count >= a) && (count <= b)

    def isValid2: Boolean =
      List(password(a - 1), password(b - 1)).count(_ == letter) == 1

  def parse(x: String): Item =
    val Pattern = """(\d+)-(\d+) (\w): (\w+)""".r
    x match
      case Pattern(a, b, letter, password) if letter.length == 1 =>
        Item(a.toInt, b.toInt, letter.head, password)
      case _ =>
        sys.error(s"Couldn't parse $x")

  val list = Source.fromResource("02.txt").getLines().map(parse).toList
  
  def solve(f: Item => Boolean) =
    println(list.count(f))
  
  List[Item => Boolean](_.isValid1, _.isValid2) foreach solve
