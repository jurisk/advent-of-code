package jurisk.adventofcode.y2023

import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import cats.implicits._
import jurisk.adventofcode.y2023.Advent10.Square._
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
    field: Field2D[Square],
    animalStartDirection: CardinalDirection2D,
  ) {
    def at(coords: Coords2D): Square =
      field.atOrElse(coords, Square.Empty)

    def successors(coords: Coords2D): List[Coords2D] = {
      val fromDirection: Map[CardinalDirection2D, Set[Square]] = Map(
        Direction2D.S -> Set(N_S, S_E, S_W),
        Direction2D.N -> Set(N_S, N_E, N_W),
        Direction2D.W -> Set(E_W, N_W, S_W),
        Direction2D.E -> Set(E_W, S_E, N_E),
      )

      def check(direction: CardinalDirection2D): List[Coords2D] = {
        val other      = coords + direction
        val otherValue = at(other)

        val turnAround = direction.rotate(Rotation.TurnAround)

        val valid = fromDirection(turnAround).contains(otherValue)
        if (valid) {
          other :: Nil
        } else {
          Nil
        }
      }

      at(coords).connections.flatMap(check)
    }
  }

  // TODO: Merge with 2018-13 Track
  sealed trait Square {
    def symbol: Char
    def connections: List[CardinalDirection2D]
  }

  case object Square {
    case object Empty extends Square {
      override def symbol: Char                           = ' '
      override def connections: List[CardinalDirection2D] = Nil
    }
    case object N_S   extends Square {
      override def symbol: Char                           = '┃'
      override def connections: List[CardinalDirection2D] = N :: S :: Nil
    }
    case object E_W   extends Square {
      override def symbol: Char                           = '━'
      override def connections: List[CardinalDirection2D] = E :: W :: Nil
    }
    case object N_E   extends Square {
      override def symbol: Char                           = '┗'
      override def connections: List[CardinalDirection2D] = N :: E :: Nil
    }
    case object N_W   extends Square {
      override def symbol: Char                           = '┛'
      override def connections: List[CardinalDirection2D] = N :: W :: Nil
    }

    case object S_W extends Square {
      override def symbol: Char                           = '┓'
      override def connections: List[CardinalDirection2D] = S :: W :: Nil
    }

    case object S_E extends Square {
      override def symbol: Char                           = '┏'
      override def connections: List[CardinalDirection2D] = S :: E :: Nil
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

      val field: Field2D[Square] = Field2D.parseFromString(
        s.replace('S', animalReplace),
        {
          case '|' => Square.N_S
          case '-' => Square.E_W
          case 'L' => Square.N_E
          case 'J' => Square.N_W
          case '7' => Square.S_W
          case 'F' => Square.S_E
          case '.' => Square.Empty
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
      // TODO: This check can be removed, Dijkstra should return it
      if (c == data.animalAt) {
        v
      } else {
        if (trackCoords.contains(c)) {
          v
        } else {
          Empty
        }
      }
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
          case Square.Empty => ???
          case Square.N_S   => direction
          case Square.E_W   => direction
          case Square.N_E   =>
            direction match {
              case Direction2D.N => ???
              case Direction2D.S => E
              case Direction2D.W => N
              case Direction2D.E => ???
            }
          case Square.N_W   =>
            direction match {
              case Direction2D.N => ???
              case Direction2D.S => W
              case Direction2D.W => ???
              case Direction2D.E => N
            }
          case Square.S_W   =>
            direction match {
              case Direction2D.N => W
              case Direction2D.S => ???
              case Direction2D.W => ???
              case Direction2D.E => S
            }
          case Square.S_E   =>
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
          case Square.Empty => Nil
          case Square.N_S   => direction.rotate(Rotation.Right90) :: Nil
          case Square.E_W   => direction.rotate(Rotation.Right90) :: Nil
          case Square.N_E   =>
            direction match {
              case Direction2D.N => NE :: Nil
              case Direction2D.S => ???
              case Direction2D.W => ???
              case Direction2D.E => W :: SW :: S :: Nil
            }
          case Square.N_W   =>
            direction match {
              case Direction2D.N => S :: SE :: E :: Nil
              case Direction2D.S => ???
              case Direction2D.W => NW :: Nil
              case Direction2D.E => ???
            }
          case Square.S_W   =>
            direction match {
              case Direction2D.N => ???
              case Direction2D.S => SW :: Nil
              case Direction2D.W => N :: NE :: E :: Nil
              case Direction2D.E => ???
            }
          case Square.S_E   =>
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
