package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.pipe.Pipe._
import jurisk.adventofcode.y2023.pipe.{CoordsWithDirection, Pipe}
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra}
import jurisk.geometry.Direction2D.{E, N, S, W}
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps

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
      val chars: Field2D[Char] = Field2D.parseFromString(s, identity)

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

      val field: Field2D[Pipe] = Field2D.parseFromString(s, mapping.apply)

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

  // Find distance to all nodes we can get to while going on the track, take the maximum
  def part1(data: Input): Int =
    Dijkstra
      .dijkstraAll(
        data.animalAt,
        (c: Coords2D) => connectedNeighbours(data.field, c).map(x => (x, 1)),
      )
      .map { case (coord @ _, (parent @ _, distance)) =>
        distance
      }
      .max

  def part2(data: Input): Int = {
    // All the track coordinates
    val trackCoords = Dijkstra
      .dijkstraAll(
        data.animalAt,
        (c: Coords2D) => connectedNeighbours(data.field, c).map(x => (x, 1)),
      )
      .keySet

    // The field with only track cells left, others are Empty
    val onlyTrack = data.field.mapByCoordsWithValues { case (c, v) =>
      if (trackCoords.contains(c)) v else Empty
    }

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

    val start = CoordsWithDirection(
      coords = topLeftFullCoord,
      direction = topLeftStartDirection,
    )

    // Which direction was the animal facing on each track segment?
    val trackCoordsWithAnimalDirection =
      Bfs.bfsReachable[CoordsWithDirection](
        start,
        x => x.nextOnTrack(data.field) :: Nil,
      )

    // Which cells were on the right of the track, as the animal was walking around it?
    val rightCoordinateSeeds = trackCoordsWithAnimalDirection
      .flatMap(x => x.coordsToTheRight(data.field).toSet)
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

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/10.txt")

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
