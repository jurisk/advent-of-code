package jurisk.adventofcode.y2023

import cats.implicits._
import jurisk.adventofcode.y2023.pipe.Pipe._
import jurisk.adventofcode.y2023.pipe.{CoordsWithDirection, Pipe}
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra}
import jurisk.geometry.Direction2D.{CardinalDirection2D, S}
import jurisk.geometry.Field2D.toDebugRepresentation
import jurisk.geometry.{Coords2D, Field2D}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._

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

  // TODO: remove `animalStartDirection`
  def part2(data: Input, animalStartDirection: CardinalDirection2D): Int = {
    val trackCoords = Dijkstra
      .dijkstraAll(
        data.animalAt,
        (c: Coords2D) => connectedNeighbours(data.field, c).map(x => (x, 1)),
      )
      .keySet

    val start = CoordsWithDirection(
      coords = data.animalAt,
      direction = animalStartDirection,
    )

    val trackCoordsWithAnimalDirection =
      Bfs.bfsReachable[CoordsWithDirection](
        start,
        x => x.nextOnTrack(data.field) :: Nil,
      )

    val rightCoords = trackCoordsWithAnimalDirection
      .flatMap(x => x.coordsToTheRight(data.field).toSet)
      .toSet
      .diff(trackCoords)

    val floodFilled = rightCoords.flatMap { c =>
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
    println(s"Part 2: ${part2(realData, S)}")
  }
}
