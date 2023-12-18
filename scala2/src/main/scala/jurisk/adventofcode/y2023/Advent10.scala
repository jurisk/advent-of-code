package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.pipe.Pipe
import jurisk.adventofcode.y2023.pipe.Pipe._
import jurisk.algorithms.pathfinding.Bfs
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.{Coords2D, CoordsAndDirection2D}
import jurisk.geometry.Direction2D.E
import jurisk.geometry.Direction2D.N
import jurisk.geometry.Direction2D.S
import jurisk.geometry.Direction2D.W
import jurisk.geometry.Field2D
import jurisk.geometry.visualizeBoolean
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

import scala.collection.immutable.ArraySeq

object Advent10 {
  final case class Input(
    animalAt: Coords2D,
    field: Field2D[Pipe],
  ) {
    def at(coords: Coords2D): Pipe =
      field.atOrElse(coords, Empty)
  }

  object Input {
    def parse(s: String): Input = {
      val chars: Field2D[Char] = Field2D.parseCharField(s)

      val animalAt = chars.filterCoordsByValue(_ == 'S').singleResultUnsafe

      val mapping = Map(
        '|' -> N_S,
        '-' -> E_W,
        'L' -> N_E,
        'J' -> N_W,
        '7' -> S_W,
        'F' -> S_E,
        '.' -> Empty,
        'S' -> Empty,
      )

      val field: Field2D[Pipe] = Field2D.parse(s, mapping.apply)

      // Find a suitable pipe to replace the `S` animal cell
      val animalPipe = Pipe.NonEmpty.filter { candidate =>
        candidate.connections.forall { direction =>
          field
            .atOrElse(animalAt + direction, Empty)
            .connections
            .contains(direction.invert)
        }
      }.singleElementUnsafe

      val updatedField = field.updatedAtUnsafe(animalAt, animalPipe)

      Input(animalAt, updatedField)
    }
  }

  def parse(input: String): Input = Input.parse(input)

  private def dijkstraToAllTrackNodes(data: Input) = Dijkstra
    .dijkstraAll(
      data.animalAt,
      (c: Coords2D) => connectedNeighbours(data.field, c).map(x => (x, 1)),
    )

  // Find distance to all nodes we can get to while going on the track, take the maximum
  def part1(data: Input): Int =
    dijkstraToAllTrackNodes(data).map {
      case (coord @ _, (parent @ _, distance)) =>
        distance
    }.max

  def part2(data: Input): Int = {
    val fromPicksShoelace        = part2PicksShoelace(data)
    val fromMarkRightSideOfTrack = part2MarkRightSideOfTrack(data)
    val from3x3Expansion         = part2From3x3Expansion(data)
    assert(
      Set(
        fromPicksShoelace,
        fromMarkRightSideOfTrack,
        from3x3Expansion,
      ).size == 1,
      s"Expected to get same results: $fromPicksShoelace and $fromMarkRightSideOfTrack and $from3x3Expansion",
    )
    fromPicksShoelace
  }

  private def findStart(onlyTrack: Field2D[Pipe]): CoordsAndDirection2D = {
    // We don't know which direction is inside and which is outside for the animal coordinates,
    // but we can figure it out for the top left coordinates. This depends on the order in which `allCoords`
    // returns coordinates.
    val topLeftFullCoord      = onlyTrack.allCoords
      .find(x => onlyTrack.atOrElse(x, Empty) != Empty)
      .getOrElse("Failed to find".fail)
    val topLeftFullValue      = onlyTrack.atOrElse(topLeftFullCoord, Empty)
    val topLeftStartDirection = topLeftFullValue match {
      case Pipe.Empty => "Unexpected".fail
      case Pipe.N_S   => S
      case Pipe.E_W   => W
      case Pipe.N_E   => N
      case Pipe.N_W   => W
      case Pipe.S_W   => S
      case Pipe.S_E   => E
    }

    CoordsAndDirection2D(
      coords = topLeftFullCoord,
      direction = topLeftStartDirection,
    )
  }

  private def walkTrack(data: Field2D[Pipe]) = {
    val start = findStart(data)

    Bfs
      .bfsReachable[CoordsAndDirection2D](
        start,
        x => nextOnTrack(x, data) :: Nil,
      )
  }

  private def extractTrack(data: Input): (Set[Coords2D], Field2D[Pipe]) = {
    // All the track coordinates
    val trackCoords = dijkstraToAllTrackNodes(data).keySet

    // The field with only track cells left, others are Empty
    val onlyTrack = data.field.mapByCoordsWithValues { case (c, v) =>
      if (trackCoords.contains(c)) v else Empty
    }

    (trackCoords, onlyTrack)
  }

  def part2PicksShoelace(data: Input): Int = {
    val (trackCoords @ _, onlyTrack) = extractTrack(data)

    // Which direction was the animal facing on each track segment?
    val trackCoordsWithAnimalDirection = walkTrack(onlyTrack)

    // Track coordinates in walking order
    val trackCoordsInWalkingOrder = ArraySeq.from(
      trackCoordsWithAnimalDirection
        .map(_.coords)
    )

    val boundaryPoints = trackCoordsInWalkingOrder.length

    // https://en.wikipedia.org/wiki/Shoelace_formula
    val area = Coords2D.areaOfSimplePolygon(trackCoordsInWalkingOrder)

    // https://en.wikipedia.org/wiki/Pick%27s_theorem
    (area - (boundaryPoints.toDouble / 2.0) + 1.0).toInt
  }

  def part2MarkRightSideOfTrack(data: Input): Int = {
    val (trackCoords, onlyTrack) = extractTrack(data)

    // Which direction was the animal facing on each track segment?
    val trackCoordsWithAnimalDirection = walkTrack(onlyTrack)

    // Which cells were on the right of the track, as the animal was walking around it?
    val rightCoordinateSeeds = trackCoordsWithAnimalDirection
      .flatMap(x => coordsToTheRight(x, data.field).toSet)
      .toSet
      .diff(trackCoords)

    // Let's flood-fill from the coordinates which we know are on the right side of the track
    val floodFilled = rightCoordinateSeeds.flatMap { c =>
      Bfs.bfsReachable[Coords2D](
        c,
        x => data.field.adjacent4(x).toSet.diff(trackCoords).toList,
      )
    }

    floodFilled.size
  }

  def part2From3x3Expansion(data: Input): Long = {
    val (_, onlyTrack) = extractTrack(data)

    val mapping: Pipe => String = {
      case Pipe.Empty =>
        """...
          |...
          |...
          |""".stripMargin
      case Pipe.N_S   =>
        """.#.
          |.#.
          |.#.
          |""".stripMargin
      case Pipe.E_W   =>
        """...
          |###
          |...
          |""".stripMargin
      case Pipe.N_E   =>
        """.#.
          |.##
          |...
          |""".stripMargin
      case Pipe.N_W   =>
        """.#.
          |##.
          |...
          |""".stripMargin
      case Pipe.S_W   =>
        """...
          |##.
          |.#.
          |""".stripMargin

      case Pipe.S_E =>
        """...
          |.##
          |.#.
          |""".stripMargin
    }

    val expanded = onlyTrack.flatMap { pipe =>
      Field2D.parseBooleanField(mapping(pipe))
    }

    Field2D.printBooleanField(expanded)

    val floodFilled = Field2D
      .floodFillField[Boolean](expanded, Coords2D.Zero, (_, to) => !to, true)

    Field2D.printBooleanField(floodFilled)

    val chunked = floodFilled.chunkIntoSubfields(3, 3)

    chunked.count(x => x.forall(_ == false))
  }

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/10.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
