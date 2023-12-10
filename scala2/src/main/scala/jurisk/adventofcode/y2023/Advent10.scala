package jurisk.adventofcode.y2023

import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import cats.implicits._
import jurisk.adventofcode.y2023.pipe.Pipe._
import jurisk.adventofcode.y2023.pipe.Pipe
import jurisk.algorithms.pathfinding.{Bfs, Dijkstra}
import jurisk.geometry.Direction2D.{
  CardinalDirection2D,
  E,
  N,
  NE,
  NW,
  S,
  SE,
  SW,
  W,
}
import jurisk.geometry.Field2D.toDebugRepresentation

object Advent10 {
  final case class Input(
    animalAt: Coords2D,
    field: Field2D[Pipe],
  ) {
    def at(coords: Coords2D): Pipe =
      field.atOrElse(coords, Pipe.Empty)
  }

  object Input {
    def parse(
      s: String,
      animalReplace: Char,
    ): Input = {
      val chars: Field2D[Char] = Field2D.parseFromString(s, identity)

      val animalAt = chars.filterCoordsByValue(_ == 'S').singleResultUnsafe

      val field: Field2D[Pipe] = Field2D.parseFromString(
        s.replace('S', animalReplace),
        {
          case '|' => Pipe.N_S
          case '-' => Pipe.E_W
          case 'L' => Pipe.N_E
          case 'J' => Pipe.N_W
          case '7' => Pipe.S_W
          case 'F' => Pipe.S_E
          case '.' => Pipe.Empty
        },
      )

      Input(animalAt, field)
    }
  }

  def parse(
    input: String,
    animalReplace: Char,
  ): Input =
    Input.parse(input, animalReplace)

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

    val onlyTrack = data.field.mapByCoordsWithValues { case (c, v) =>
      if (trackCoords.contains(c)) v else Empty
    }

    final case class CoordsWithDirection(
      coords: Coords2D,
      direction: CardinalDirection2D,
    ) {
      def nextOnTrack: CoordsWithDirection = {
        val nextCoords = coords + direction
        val nextSquare = data.at(nextCoords)

        val nextDirection = nextSquare.connections
          .filterNot(_ == direction.invert)
          .singleElementUnsafe

        CoordsWithDirection(
          coords = nextCoords,
          direction = nextDirection,
        )
      }

      def coordsToTheRight: List[Coords2D] = {
        val diffs: List[Direction2D] = data.at(coords) match {
          case Pipe.Empty => Nil

          case Pipe.N_S | Pipe.E_W =>
            direction.rotate(Rotation.Right90) :: Nil

          case Pipe.N_E =>
            direction match {
              case Direction2D.E => W :: SW :: S :: Nil
              case _             => Nil
            }
          case Pipe.N_W =>
            direction match {
              case Direction2D.N => S :: SE :: E :: Nil
              case _             => Nil
            }
          case Pipe.S_W =>
            direction match {
              case Direction2D.W => N :: NE :: E :: Nil
              case _             => Nil
            }
          case Pipe.S_E =>
            direction match {
              case Direction2D.S => N :: NW :: W :: Nil
              case _             => Nil
            }
        }

        diffs.map(x => coords + x)
      }
    }

    val start = CoordsWithDirection(
      coords = data.animalAt,
      direction = animalStartDirection,
    )

    val trackCoordsWithAnimalDirection =
      Bfs.bfsReachable[CoordsWithDirection](start, x => x.nextOnTrack :: Nil)

    val trackCarets = onlyTrack
      .mapByCoordsWithValues { case (c, _) =>
        trackCoordsWithAnimalDirection.find(_.coords == c) match {
          case Some(value) => value.direction.toCaret
          case None        => ' '
        }
      }

    val rightCoords = trackCoordsWithAnimalDirection
      .flatMap(x => x.coordsToTheRight.toSet)
      .toSet
      .filter(x => onlyTrack.at(x).contains(Empty))

    val seeds: Field2D[Char] = trackCarets.mapByCoordsWithValues {
      case (c, v) => if (rightCoords.contains(c)) '█' else v
    }

    println(toDebugRepresentation(seeds))

    // TODO: extract floodFill as an algorithm
    val floodFilled = rightCoords.flatMap { c =>
      Bfs.bfsReachable[Coords2D](
        c,
        x =>
          onlyTrack
            .neighboursFor(x, includeDiagonal = false)
            .filter(n => onlyTrack.at(n).contains(Empty)),
      )
    }

    floodFilled.size
  }

  def parseFile(
    fileName: String,
    animalReplace: Char,
  ): Input =
    parse(readFileText(fileName), animalReplace)

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/10.txt", '7')

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData, S)}")
  }
}
