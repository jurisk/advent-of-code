package jurisk.adventofcode.y2018

import cats.data.NonEmptyList
import cats.implicits._
import jurisk.adventofcode.y2018.Advent20.Command.Branch
import jurisk.adventofcode.y2018.Advent20.Command.Move
import jurisk.algorithms.pathfinding.Bfs
import jurisk.algorithms.pathfinding.Dijkstra
import jurisk.geometry.Coords2D
import jurisk.geometry.Direction2D._
import jurisk.utils.FileInput._
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable

object Advent20 {
  final case class Door private (
    private val from: Coords2D,
    private val to: Coords2D,
  ) {
    def pairs: Set[(Coords2D, Coords2D)] = Set((from, to), (to, from))
  }

  object Door {
    def apply(a: Coords2D, b: Coords2D): Door = {
      assert(a != b)
      new Door(
        Coords2D.readingOrdering.min(a, b),
        Coords2D.readingOrdering.max(a, b),
      )
    }
  }

  final case class RegEx(commands: Vector[Command]) {
    override def toString: String = commands.map(_.toString).mkString
  }

  sealed trait Command

  object Command {
    final case class Move(direction: CardinalDirection2D) extends Command {
      override def toString: String = direction.toString
    }

    final case class Branch(
      options: NonEmptyList[RegEx]
    ) extends Command {
      override def toString: String = options.toList.mkString("(", "|", ")")
    }
  }

  def parse(input: String): RegEx = {
    import cats.parse.{Parser => P}

    val moveN: P[Move]      = P.char('N').map(_ => Move(N))
    val moveS: P[Move]      = P.char('S').map(_ => Move(S))
    val moveE: P[Move]      = P.char('E').map(_ => Move(E))
    val moveW: P[Move]      = P.char('W').map(_ => Move(W))
    val moveParser: P[Move] = moveN | moveS | moveE | moveW

    val regExParser: P[RegEx] = P.recursive[RegEx] { recurse =>
      // Must be .recursive to avoid a StackOverFlowException
      val branchParser: P[Branch] = {
        val maybeEmpty = recurse.?.map(_.getOrElse(RegEx(Vector.empty)))
        val regExList  = (maybeEmpty ~ (P.char('|') *> maybeEmpty).rep0)
          .map { case (head, tail) => NonEmptyList(head, tail) }

        (P.char('(') *> regExList <* P.char(')')).map(Branch)
      }

      val commandParser: P[Command] = moveParser | branchParser

      commandParser.rep.map(x => RegEx(x.toList.toVector))
    }

    val rootParser: P[RegEx] = regExParser.between(P.char('^'), P.char('$'))

    rootParser.parseAll(input) match {
      case Left(error)  =>
        sys.error(
          s"Error $error at '${error.input.getOrElse("").drop(error.failedAtOffset)}'"
        )
      case Right(value) => value
    }
  }

  def solve(input: RegEx): (Int, Int) = {
    val doors: mutable.Set[Door] = mutable.Set.empty

    def successors(
      input: (Vector[Command], Coords2D)
    ): List[(Vector[Command], Coords2D)] = {
      val (commands, coords) = input

      commands match {
        case Branch(options) +: next =>
          options.toList flatMap { option =>
            List((option.commands ++ next, coords))
          }

        case Move(direction) +: next =>
          val newCoords = coords + direction
          doors.add(Door(coords, newCoords))
          if (doors.size % 1000 == 0) println(s"${doors.size} doors added...")
          List((next, newCoords))

        case _ => Nil
      }
    }

    def visit(ignored: (Vector[Command], Coords2D)): Unit = {}

    Bfs.bfsVisitAll((input.commands, Coords2D.Zero), successors, visit)

    println(s"${doors.size} doors")

    val edges: mutable.Map[Coords2D, Set[Coords2D]] = mutable.Map.empty

    doors foreach { door =>
      door.pairs foreach { case (a, b) =>
        edges.updateWith(a) { v =>
          (v.getOrElse(Set.empty) + b).some
        }
      }
    }

    val results = Dijkstra.dijkstraAll(
      Coords2D.Zero,
      (c: Coords2D) => edges(c).toList.map(n => (n, 1)),
    )

    val lengths = results.values.map { case (_, len) => len }

    val result1 = lengths.max
    val result2 = lengths.count(_ >= 1000)

    (result1, result2)
  }

  def main(args: Array[String]): Unit = {
    val realData = readFileText("2018/20.txt")

    val real = parse(realData)

    solve(parse("^WNE$"))._1 shouldEqual 3
    solve(parse("^ENWWW(NEEE|SSE(EE|N))$"))._1 shouldEqual 10
    solve(parse("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"))._1 shouldEqual 18
    solve(
      parse("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
    )._1 shouldEqual 23
    solve(
      parse("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
    )._1 shouldEqual 31

    val (result1, result2) = solve(real)
    result1 shouldEqual 3835
    result2 shouldEqual 8520
  }
}
