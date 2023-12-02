package jurisk.adventofcode.y2023

import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

object Advent02 {
  type Parsed = List[Game]

  private type GameId = Int

  final case class Game(
    id: GameId,
    observations: List[Cubes],
  ) {
    private val max: Cubes = {
      def extract(f: Cubes => Int): Int =
        observations.map(f).maxOption.getOrElse(0)

      Cubes(
        extract(_.red),
        extract(_.green),
        extract(_.blue),
      )
    }

    def possible(bag: Cubes): Boolean =
      max.possible(bag)

    def power: Int =
      max.red * max.green * max.blue
  }

  final case class Cubes(red: Int, blue: Int, green: Int) {
    def possible(bag: Cubes): Boolean =
      bag.red >= red && bag.green >= green && bag.blue >= blue

    override def toString: String =
      s"R$red G$green B$blue"
  }

  private object Cubes {
    private def empty: Cubes = Cubes(0, 0, 0)

    def parse(s: String): Cubes =
      s.split(", ").foldLeft(Cubes.empty) { case (a, b) =>
        b match {
          case s"$red red"     => a.copy(red = red.toInt)
          case s"$green green" => a.copy(green = green.toInt)
          case s"$blue blue"   => a.copy(blue = blue.toInt)
          case _               => s.failedToParse
        }
      }
  }

  private object Game {
    def parse(s: String): Game =
      s match {
        case s"Game $id: $rem" =>
          Game(id.toInt, rem.split("; ").map(Cubes.parse).toList)
        case _                 => s.failedToParse
      }
  }

  def parse(input: String): Parsed =
    input.parseList("\n", Game.parse)

  def part1(data: Parsed): Int = {
    val bag = Cubes(12, 13, 14)

    data
      .filter(_ possible bag)
      .map(_.id)
      .sum
  }

  def part2(data: Parsed): Int =
    data
      .map(_.power)
      .sum

  def parseFile(fileName: String): Parsed =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Parsed = parseFile("2023/02.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
