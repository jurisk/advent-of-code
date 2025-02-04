package jurisk.adventofcode.y2016

import cats.Endo
import cats.effect.IO
import cats.effect.IOApp
import cats.syntax.all._
import jurisk.adventofcode.y2016.Advent24.Square.LocationCode
import jurisk.adventofcode.y2016.Advent24.Square.Zero
import jurisk.algorithms.pathfinding.Bfs
import jurisk.collections.immutable.SetOfTwo
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInputIO.readFileText
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

object Advent24 extends IOApp.Simple {
  final case class Ducts(private val field: Field2D[Square]) {
    private val allCodes: Set[LocationCode] =
      field.values.flatMap {
        case Square.Empty           => None
        case Square.Wall            => None
        case Square.Location(value) => value.some
      }.toSet

    private val nonZeroCodes: Set[LocationCode] =
      allCodes - Zero

    private val distances: Map[SetOfTwo[LocationCode], Int] = {
      def distanceBetweenTwoLocations(
        setOfTwo: SetOfTwo[LocationCode]
      ): Option[Int] = {
        def isPassable(c: Coords2D): Boolean =
          field.atOrElse(c, Square.Wall) != Square.Wall

        def locationOf(locationCode: LocationCode): Coords2D =
          field
            .filterCoordsByValue(Square.Location(locationCode))
            .singleElementUnsafe

        val (a, b) = setOfTwo.tupleInArbitraryOrder
        val target = locationOf(b)
        Bfs.bfsLength[Coords2D](
          locationOf(a),
          c => field.adjacent4(c).filter(isPassable),
          x => x == target,
        )
      }

      allCodes.toList
        .combinations(2)
        .map { combination: List[LocationCode] =>
          SetOfTwo.fromList(combination)
        }
        .flatMap { key =>
          distanceBetweenTwoLocations(key).map(key -> _)
        }
        .toMap
    }

    def solve(f: Endo[List[LocationCode]]): Int = {
      def costOfPath(path: List[LocationCode]): Int = (path.init zip path.tail)
        .map { case (a, b) => SetOfTwo(a, b) }
        .map(distances)
        .sum

      nonZeroCodes.toList.permutations
        .map(f)
        .map(costOfPath)
        .min
    }
  }

  object Ducts {
    def parse(input: String): Ducts =
      Ducts(Field2D.parse(input, Square.parse))
  }

  sealed trait Square
  object Square {
    final case class LocationCode(code: Int) extends AnyVal {
      override def toString: String = code.toString
    }

    val Zero: LocationCode = LocationCode(0)

    case object Empty                              extends Square
    case object Wall                               extends Square
    final case class Location(value: LocationCode) extends Square

    def parse(ch: Char): Square =
      ch match {
        case '.'            => Square.Empty
        case '#'            => Square.Wall
        case x if x.isDigit => Square.Location(LocationCode(x - '0'))
        case _              => ch.toString.failedToParse
      }
  }

  private def parseAndSolve(
    input: String,
    f: Endo[List[LocationCode]],
  ): Int =
    Ducts.parse(input).solve(f)

  private def part1(input: String): Int =
    parseAndSolve(input, permutation => Zero :: permutation)

  private def part2(input: String): Int =
    parseAndSolve(input, permutation => Zero :: permutation ::: (Zero :: Nil))

  override def run: IO[Unit] = for {
    testData   <- readFileText("2016/24-test.txt")
    testResult1 = part1(testData)
    _           = testResult1 shouldEqual 14
    realData   <- readFileText("2016/24.txt")
    realResult1 = part1(realData)
    _          <- IO.println(s"Part 1: $realResult1")
    _           = realResult1 shouldEqual 428
    realResult2 = part2(realData)
    _          <- IO.println(s"Part 2: $realResult2")
    _           = realResult2 shouldEqual 680
  } yield ()
}
