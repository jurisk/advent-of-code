package jurisk.adventofcode.y2023

import jurisk.geometry.{Coords2D, Direction2D, Field2D, Rotation}
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import cats.implicits._
import jurisk.adventofcode.y2023.Advent10.Square
import jurisk.adventofcode.y2023.Advent10.Square._
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.Direction2D.CardinalDirection2D
import jurisk.geometry.Field2D.toDebugRepresentation

object Advent10 {
  final case class Input(
    animalAt: Coords2D,
    field: Field2D[Square],
  ) {
    def at(coords: Coords2D): Square =
      field.atOrElse(coords, Square.Empty)

    def successors(coords: Coords2D): List[Coords2D] = {
      val fromDirection: Map[CardinalDirection2D, Set[Square]] = Map(
        Direction2D.S -> Set(N_S, SE, SW),
        Direction2D.N -> Set(N_S, NE, NW),
        Direction2D.W -> Set(E_W, NW, SW),
        Direction2D.E -> Set(E_W, SE, NE),
      )

      def check(direction: CardinalDirection2D): List[Coords2D] = {
        val turnAround       = direction.rotate(Rotation.TurnAround)
        val set: Set[Square] = fromDirection(turnAround)
        val other            = coords + direction
        val otherValue       = at(other)
        val valid            = set.contains(otherValue)
        if (valid) {
          other :: Nil
        } else {
          Nil
        }
      }

      at(coords) match {
        case Square.Empty => Nil
        case Square.N_S   => check(Direction2D.N) ::: check(Direction2D.S)
        case Square.E_W   => check(Direction2D.E) ::: check(Direction2D.W)
        case Square.SE    => check(Direction2D.S) ::: check(Direction2D.E)
        case Square.NE    => check(Direction2D.N) ::: check(Direction2D.E)
        case Square.NW    => check(Direction2D.N) ::: check(Direction2D.W)
        case Square.SW    => check(Direction2D.S) ::: check(Direction2D.W)
      }
    }
  }

  sealed trait Square {
    def symbol: Char
  }
  case object Square  {
    case object Empty extends Square {
      override def symbol: Char = ' '
    }
    case object N_S   extends Square {
      override def symbol: Char = '|'
    }
    case object E_W   extends Square {
      override def symbol: Char = '-'
    }
    case object NE    extends Square {
      override def symbol: Char = 'L'
    }
    case object NW    extends Square {
      override def symbol: Char = 'J'
    }

    case object SW extends Square {
      override def symbol: Char = '7'
    }

    case object SE extends Square {
      override def symbol: Char = 'F'
    }

  }

  object Input {
    def parse(s: String, animalReplace: Char): Input = {
      val chars: Field2D[Char] = Field2D.parseFromString(s, identity)

      val animalAt = chars.filterCoordsByValue(_ == 'S').singleResultUnsafe

      val field: Field2D[Square] = Field2D.parseFromString(
        s.replace('S', animalReplace),
        {
          case '|' => Square.N_S
          case '-' => Square.E_W
          case 'L' => Square.NE
          case 'J' => Square.NW
          case '7' => Square.SW
          case 'F' => Square.SE
          case '.' => Square.Empty
        },
      )

      Input(animalAt, field)
    }
  }

  def parse(input: String, animalReplace: Char): Input =
    Input.parse(input, animalReplace)

  def part1(data: Input): Int = {
    val qq = Dijkstra
      .dijkstraAll(
        data.animalAt,
        (c: Coords2D) => data.successors(c).map(x => (x, 1)),
      )

    val gugu = data.field.map(_.symbol)
    println(toDebugRepresentation(gugu))

    val deb = gugu.mapByCoordsWithValues { case (c, _) =>
      qq.get(c) match {
        case Some((_, n)) => n.toString.last
        case None         => '.'
      }
    }

    println(toDebugRepresentation(deb))

    qq.values
      .map(_._2)
      .max
  }

  def part2(data: Input): Int = {
    val qq = Dijkstra
      .dijkstraAll(
        data.animalAt,
        (c: Coords2D) => data.successors(c).map(x => (x, 1)),
      )

    val gugu = data.field.map(_.symbol)

    val deb = gugu
      .mapByCoordsWithValues { case (c, _) =>
        if (c == data.animalAt) {
          true
        } else {
          qq.get(c) match {
            case Some((_, _)) => true
            case None         => false
          }
        }
      }
      .map { v =>
        if (v) {
          '█'
        } else {
          '░'
        }
      }

    println(toDebugRepresentation(deb))

    0
  }

  def parseFile(fileName: String, animalReplace: Char): Input =
    parse(readFileText(fileName), animalReplace)

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/10.txt", '7')

    println(s"Part 1: ${part1(realData)}")
    println(s"Part 2: ${part2(realData)}")
  }
}
