package jurisk.adventofcode.y2023

import cats.implicits._
import com.microsoft.z3.BoolExpr
import jurisk.adventofcode.y2023.Advent16.Square.Empty
import jurisk.collections.immutable.BiMap
import jurisk.collections.immutable.BiMap.BiDirectionalArrowAssociation
import jurisk.geometry.Coords2D
import jurisk.geometry.CoordsAndDirection2D
import jurisk.geometry.Direction2D
import jurisk.geometry.Direction2D._
import jurisk.geometry.Field2D
import jurisk.optimization.ImplicitConversions.RichBoolExpr
import jurisk.optimization.ImplicitConversions.RichExpr
import jurisk.optimization.ImplicitConversions.RichExprBoolSort
import jurisk.optimization.ImplicitConversions.RichString
import jurisk.optimization.Optimizer
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Simulation
import mouse.all.booleanSyntaxMouse

import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq

object Advent16 {
  type Input = Field2D[Square]

  sealed trait Square extends Product with Serializable {
    def asChar: Char = Square.Mapping.rightToLeftUnsafe(this)

    def incomingToOutgoing(
      incoming: CardinalDirection2D
    ): Set[CardinalDirection2D] = this match {
      case Square.Empty              =>
        Set(incoming.invert)
      case Square.MirrorLeanRight    =>
        Set(incoming match {
          case N => W
          case E => S
          case S => E
          case W => N
        })
      case Square.MirrorLeanLeft     =>
        Set(incoming match {
          case N => E
          case E => N
          case S => W
          case W => S
        })
      case Square.HorizontalSplitter =>
        incoming match {
          case N | S => Set(E, W)
          case ew    => Set(ew.invert)
        }
      case Square.VerticalSplitter   =>
        incoming match {
          case E | W => Set(N, S)
          case ns    => Set(ns.invert)
        }
    }
  }

  object Square {
    val Mapping: BiMap[Char, Square] = BiMap(
      '.' <-> Empty,
      '/' <-> MirrorLeanRight,
      '\\' <-> MirrorLeanLeft,
      '-' <-> HorizontalSplitter,
      '|' <-> VerticalSplitter,
    )

    case object Empty              extends Square
    case object MirrorLeanRight    extends Square
    case object MirrorLeanLeft     extends Square
    case object HorizontalSplitter extends Square
    case object VerticalSplitter   extends Square
  }

  def parse(input: String): Input =
    Field2D.parseFromBiMap(
      input,
      Square.Mapping,
    )

  final case class State(
    incomingQueue: Set[CoordsAndDirection2D],
    outgoingQueue: Set[CoordsAndDirection2D],
    incomingEdgesProcessed: Set[CoordsAndDirection2D],
  ) {
    def next(field: Input): State = {
      val newIncomingQueue = outgoingQueue.flatMap { next =>
        val neighbour = next.nextStraight
        field.isValidCoordinate(neighbour.coords) option
          neighbour.invertDirection
      } -- incomingEdgesProcessed

      val newOutgoingQueue = for {
        next     <- incomingQueue
        outgoing <- field
                      .at(next.coords)
                      .toList
                      .flatMap(_.incomingToOutgoing(next.direction))
      } yield next.copy(direction = outgoing)

      State(
        newIncomingQueue,
        newOutgoingQueue,
        incomingEdgesProcessed ++ newIncomingQueue,
      )
    }
  }

  object State {
    def fromInitial(
      initial: CoordsAndDirection2D
    ): State = {
      val incomingQueue = Set(initial)
      State(
        incomingQueue = incomingQueue,
        outgoingQueue = Set.empty,
        incomingEdgesProcessed = incomingQueue,
      )
    }
  }

  private def solveBySimulation(
    field: Input,
    initial: CoordsAndDirection2D,
  ): Long = {
    val initialState = State.fromInitial(initial)
    val endState     = Simulation.runUntilStableState(initialState)(_.next(field))
    endState.incomingEdgesProcessed.map(_.coords).size
  }

  sealed trait MinimizeOrMaximize
  object MinimizeOrMaximize {
    case object Minimize extends MinimizeOrMaximize
    case object Maximize extends MinimizeOrMaximize
  }

  private[y2023] def solveByOptimization(
    field: Input,
    initial: Option[CoordsAndDirection2D],
    optimizationDirection: MinimizeOrMaximize,
    debug: Boolean = false,
  ): Long = {
    implicit val optimizer: Optimizer = Optimizer.z3()
    import optimizer._

    def boolExpr(c: Coords2D, direction: CardinalDirection2D, prefix: String) =
      s"${prefix}_${c.x}_${c.y}_${direction.asString}".labeledBool

    def incomingBool(c: Coords2D, direction: CardinalDirection2D) =
      boolExpr(c, direction, "incoming")

    def outgoingBool(c: Coords2D, direction: CardinalDirection2D) =
      boolExpr(c, direction, "outgoing")

    // Outgoing equals incoming in neighbour
    field.allConnectionsDirectional.foreach { case (from, direction, to) =>
      addConstraints(
        outgoingBool(from, direction) === incomingBool(to, direction.invert)
      )
    }

    var outgoingConstraintsQueue: List[(BoolExpr, BoolExpr)] = List.empty

    // Incoming implies outgoing (depending on square type)
    field.mapByCoordsWithValues { case (c, sq) =>
      Direction2D.CardinalDirections foreach { incomingDirection =>
        sq.incomingToOutgoing(incomingDirection) foreach { outgoingDirection =>
          val in  = incomingBool(c, incomingDirection)
          val out = outgoingBool(c, outgoingDirection)

          outgoingConstraintsQueue = (in -> out) :: outgoingConstraintsQueue

          addConstraints(in ==> out)
        }
      }
    }

    // We can only have an outgoing if we have one of the relevant incomings
    outgoingConstraintsQueue
      .groupMap(_._2)(_._1)
      .foreach { case (out, ins) =>
        addConstraints(
          out === or(ins: _*)
        )
      }

    field.mapByCoordsWithValues { case (c, v) =>
      val directions = v match {
        case Square.HorizontalSplitter => N :: S :: Nil
        case Square.VerticalSplitter   => E :: W :: Nil
        case _                         => Nil
      }

      directions foreach { direction =>
        addConstraints(
          outgoingBool(c, direction) === False
        )
      }
    }

    // Exactly one of the incomings from the edge is 1
    val allEdgeIncomings = edgeIncomings(field).map { edge =>
      incomingBool(edge.coords, edge.direction).toInt
    }.toSeq

    assert(allEdgeIncomings.distinct.length == (field.height + field.width) * 2)

    addConstraints(
      sum(allEdgeIncomings: _*) === One
    )

    // Only for Part 1 - initialSquare incoming initialDirection is 1, others are 0
    initial foreach { initial =>
      addConstraints(
        incomingBool(initial.coords, initial.direction) === True
      )
    }

    // `energized` is sum of all squares which have incoming
    val energizedVar = labeledInt("energized")
    addConstraints(
      energizedVar === sum(
        field.allCoords.map { c =>
          val incomings = Direction2D.CardinalDirections.map(incomingBool(c, _))
          or(incomings: _*).toInt
        }: _*
      )
    )

    // Note - I hoped that we can solve Part 2 in one go if we maximise, but the loops in the middle got turned on then,
    // leading to results that were too high.

    val objective = optimizationDirection match {
      case MinimizeOrMaximize.Minimize => minimize(energizedVar)
      case MinimizeOrMaximize.Maximize => maximize(energizedVar)
    }

    if (debug) {
      optimizer.debugPrint()
    }

    if (debug) {
      println(s"Objective: $objective")
      println(s"Lower:\n${objective.getLower}")
      println(s"Upper:\n${objective.getUpper}")
    }

    if (debug) {
      @nowarn("cat=deprecation")
      val model = checkAndGetModel()
      println(s"Model:\n$model")

      val debugField = field
        .mapByCoords { c =>
          def f(d: CardinalDirection2D): Char = {
            val incoming = incomingBool(c, d)
            val outgoing = outgoingBool(c, d)

            @nowarn("cat=deprecation")
            val incm = extractBoolean(incoming).getOrElse("Unknown".fail)

            @nowarn("cat=deprecation")
            val outg = extractBoolean(outgoing).getOrElse("Unknown".fail)

            (incm, outg) match {
              case (false, false) => ' '
              case (true, false)  => 'I'
              case (false, true)  => 'O'
              case (true, true)   => 'B'
            }
          }

          Field2D(
            ArraySeq(
              ArraySeq(' ', '┄', '┄', '┄', ' '),
              ArraySeq('┆', ' ', f(N), ' ', '┆'),
              ArraySeq('┆', f(W), field.atOrElse(c, Empty).asChar, f(E), '┆'),
              ArraySeq('┆', ' ', f(S), ' ', '┆'),
              ArraySeq(' ', '┄', '┄', '┄', ' '),
            )
          )
        }
        .flatMap(identity)
        .map(ch => if (ch == '.') ' ' else ch)

      Field2D.printCharField(debugField)
    }

    val List(result) = runExternal("energized").getOrElse("Failed".fail)
    resultToLong(result)
  }

  private def edgeIncomings(
    field: Input
  ): Iterable[CoordsAndDirection2D] =
    field.topRowCoords.map(CoordsAndDirection2D(_, N)) :::
      field.rightColumnCoords.map(CoordsAndDirection2D(_, E)) :::
      field.bottomRowCoords.map(CoordsAndDirection2D(_, S)) :::
      field.leftColumnCoords.map(CoordsAndDirection2D(_, W))

  private[y2023] def part1Simulation(field: Input): Long =
    solveBySimulation(field, CoordsAndDirection2D(Coords2D.Zero, Direction2D.W))

  private[y2023] def part1Optimization(field: Input): Long =
    solveByOptimization(
      field,
      CoordsAndDirection2D(Coords2D.Zero, Direction2D.W).some,
      MinimizeOrMaximize.Minimize,
    )

  private[y2023] def part2Simulation(field: Input): Long = {
    val solutions = edgeIncomings(field) map { initial =>
      solveBySimulation(field, initial)
    }

    solutions.max
  }

  // Note - this fails due to loops being "turned on"
  private[y2023] def part2Optimization(field: Input): Long =
    solveByOptimization(field, none, MinimizeOrMaximize.Maximize, debug = false)

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def main(args: Array[String]): Unit = {
    val realData: Input = parseFile("2023/16.txt")

    println(s"Part 1: ${part1Simulation(realData)}")
    println(s"Part 2: ${part2Simulation(realData)}")
  }
}
