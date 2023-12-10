package jurisk.adventofcode.y2023

import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import cats.implicits._
import jurisk.adventofcode.y2023.Advent10.Pipe._
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
    animalStartDirection: CardinalDirection2D,
  ) {
    def at(coords: Coords2D): Pipe =
      field.atOrElse(coords, Pipe.Empty)

    def successors(coords: Coords2D): List[Coords2D] =
      at(coords).connections
        .filter { direction =>
          at(coords + direction).connections.contains(direction.invert)
        }
        .map { direction =>
          coords + direction
        }
        .toList
  }

  sealed trait Pipe {
    def symbol: Char
    def connections: Set[CardinalDirection2D]
  }

  case object Pipe {
    case object Empty extends Pipe {
      override def symbol: Char                          = ' '
      override def connections: Set[CardinalDirection2D] = Set.empty
    }

    case object N_S extends Pipe {
      override def symbol: Char                          = '┃'
      override def connections: Set[CardinalDirection2D] = Set(N, S)
    }

    case object E_W extends Pipe {
      override def symbol: Char                          = '━'
      override def connections: Set[CardinalDirection2D] = Set(E, W)
    }

    case object N_E extends Pipe {
      override def symbol: Char                          = '┗'
      override def connections: Set[CardinalDirection2D] = Set(N, E)
    }

    case object N_W extends Pipe {
      override def symbol: Char                          = '┛'
      override def connections: Set[CardinalDirection2D] = Set(N, W)
    }

    case object S_W extends Pipe {
      override def symbol: Char                          = '┓'
      override def connections: Set[CardinalDirection2D] = Set(S, W)
    }

    case object S_E extends Pipe {
      override def symbol: Char                          = '┏'
      override def connections: Set[CardinalDirection2D] = Set(S, E)
    }
  }

  object Input {
    def parse(
      s: String,
      animalReplace: Char,
      animalStartDirection: CardinalDirection2D,
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

      Input(animalAt, field, animalStartDirection)
    }
  }

  def parse(
    input: String,
    animalReplace: Char,
    animalStartDirection: CardinalDirection2D,
  ): Input =
    Input.parse(input, animalReplace, animalStartDirection)

  def part1(data: Input): Int =
    Dijkstra
      .dijkstraAll(
        data.animalAt,
        (c: Coords2D) => data.successors(c).map(x => (x, 1)),
      )
      .map { case (coord @ _, (parent @ _, distance)) =>
        distance
      }
      .max

  def part2(data: Input): Int = {
    val trackCoords = Dijkstra
      .dijkstraAll(
        data.animalAt,
        (c: Coords2D) => data.successors(c).map(x => (x, 1)),
      )
      .keySet

    val onlyTrack = data.field.mapByCoordsWithValues { case (c, v) =>
      if (trackCoords.contains(c)) v else Empty
    }

    // TODO: Merge with 2018-13 Cart
    final case class CoordsWithDirection(
      coords: Coords2D,
      direction: CardinalDirection2D,
    ) {
      def next: CoordsWithDirection = {
        val nextCoords = coords + direction
        val nextSquare = data.at(nextCoords)

        val nextDirection = nextSquare match {
          case Pipe.Empty => ???
          case Pipe.N_S   => direction
          case Pipe.E_W   => direction
          case Pipe.N_E   =>
            direction match {
              case Direction2D.N => ???
              case Direction2D.S => E
              case Direction2D.W => N
              case Direction2D.E => ???
            }
          case Pipe.N_W   =>
            direction match {
              case Direction2D.N => ???
              case Direction2D.S => W
              case Direction2D.W => ???
              case Direction2D.E => N
            }
          case Pipe.S_W   =>
            direction match {
              case Direction2D.N => W
              case Direction2D.S => ???
              case Direction2D.W => ???
              case Direction2D.E => S
            }
          case Pipe.S_E   =>
            direction match {
              case Direction2D.N => E
              case Direction2D.S => ???
              case Direction2D.W => S
              case Direction2D.E => ???
            }
        }

        CoordsWithDirection(
          coords = nextCoords,
          direction = nextDirection,
        )
      }

      def coordsToTheRight: List[Coords2D] = {
        val directions: List[Direction2D] = data.at(coords) match {
          case Pipe.Empty => Nil
          case Pipe.N_S   => direction.rotate(Rotation.Right90) :: Nil
          case Pipe.E_W   => direction.rotate(Rotation.Right90) :: Nil
          case Pipe.N_E   =>
            direction match {
              case Direction2D.N => NE :: Nil
              case Direction2D.S => ???
              case Direction2D.W => ???
              case Direction2D.E => W :: SW :: S :: Nil
            }
          case Pipe.N_W   =>
            direction match {
              case Direction2D.N => S :: SE :: E :: Nil
              case Direction2D.S => ???
              case Direction2D.W => NW :: Nil
              case Direction2D.E => ???
            }
          case Pipe.S_W   =>
            direction match {
              case Direction2D.N => ???
              case Direction2D.S => SW :: Nil
              case Direction2D.W => N :: NE :: E :: Nil
              case Direction2D.E => ???
            }
          case Pipe.S_E   =>
            direction match {
              case Direction2D.N => ???
              case Direction2D.S => N :: NW :: W :: Nil
              case Direction2D.W => ???
              case Direction2D.E => SE :: Nil
            }
        }

        directions.map(x => coords + x)
      }
    }

    val start = CoordsWithDirection(
      coords = data.animalAt,
      direction = data.animalStartDirection,
    )

    val trackCoordsWithAnimalDirection =
      Bfs.bfsReachable[CoordsWithDirection](start, x => x.next :: Nil)

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
      case (c, v) =>
        if (rightCoords.contains(c)) {
          '█'
        } else {
          v
        }
    }

    println(toDebugRepresentation(seeds))

    // TODO: extract floodFill as an algorithm
    val floodFilled = rightCoords.flatMap { c =>
      Bfs.bfsReachable[Coords2D](
        c,
        x =>
          onlyTrack
            .neighboursFor(x, includeDiagonal = false)
            .filter(n => onlyTrack.atOrElse(n, Empty) == Empty),
      )
    }

    floodFilled.size
  }

  def parseFile(
    fileName: String,
    animalReplace: Char,
    animalStartDirection: CardinalDirection2D,
  ): Input =
    parse(readFileText(fileName), animalReplace, animalStartDirection)

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/10.txt", '7', Direction2D.W)

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
