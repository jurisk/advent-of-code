package jurisk.adventofcode.y2022

import cats.implicits._
import jurisk.adventofcode.y2022.Advent17.Square.Empty
import jurisk.algorithms.pathfinding.Bfs
import jurisk.geometry.{Coords2D, Direction2D, Field2D}
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import org.scalatest.matchers.should.Matchers._

import scala.collection.mutable

object Advent17 {
  sealed trait JetMove {
    def toChar: Char = this match {
      case JetMove.Left  => '>'
      case JetMove.Right => '<'
    }

    override def toString: String = this match {
      case JetMove.Left  => "Left"
      case JetMove.Right => "Right"
    }

    def diff: Coords2D = this match {
      case JetMove.Left  => Direction2D.W.diff
      case JetMove.Right => Direction2D.E.diff
    }
  }
  object JetMove       {
    object Left  extends JetMove
    object Right extends JetMove
  }

  type JetPattern = Vector[JetMove]
  def parse(data: String): JetPattern =
    data.map {
      case '>' => JetMove.Right
      case '<' => JetMove.Left
    }.toVector

  sealed trait Square
  object Square {
    case object Wall  extends Square
    case object Rock  extends Square
    case object Empty extends Square
  }

  final case class RockPattern(coords: Set[Coords2D])
  object RockPattern {
    import Coords2D.Zero

    val Horizontal = RockPattern(
      Set(
        Zero,
        Zero.E,
        Zero.E.E,
        Zero.E.E.E,
      )
    )
    val Cross      = RockPattern(
      Set(
        Zero.N,
        Zero.E,
        Zero.NE,
        Zero.NE.N,
        Zero.NE.E,
      )
    )
    val ReverseL   = RockPattern(
      Set(
        Zero,
        Zero.E,
        Zero.E.E,
        Zero.E.NE,
        Zero.NE.NE,
      )
    )
    val Vertical   = RockPattern(
      Set(
        Zero,
        Zero.N,
        Zero.N.N,
        Zero.N.N.N,
      )
    )
    val Block      = RockPattern(
      Set(
        Zero,
        Zero.N,
        Zero.E,
        Zero.NE,
      )
    )

    val RockPatterns: Array[RockPattern] =
      Array(Horizontal, Cross, ReverseL, Vertical, Block)
  }

  def hitsFieldOrSides(
    field: Field2D[Square],
    rockCoords: Set[Coords2D],
  ): Boolean =
    !rockCoords.forall(c => field.at(c).contains(Empty))

  def fieldMinY(field: Field2D[Square]): Int =
    field
      .filterCoordsByValue {
        case Square.Wall  => true
        case Square.Rock  => true
        case Square.Empty => false
      }
      .map(_.y.value)
      .min

  private def printField(
    field: Field2D[Square],
    rockCoords: Set[Coords2D] = Set.empty,
  ): Unit = {
    val adjusted = rockCoords.foldLeft(field) { case (acc, c) =>
      acc.updatedAtUnsafe(c, Square.Rock)
    }

    val charField = adjusted.map {
      case Square.Wall  => '#'
      case Square.Rock  => '@'
      case Square.Empty => '.'
    }

    val asString = Field2D.toDebugRepresentation(charField)
    println(asString)
    println
  }

  val Debug = false
  private def dropRock(
    field: Field2D[Square],
    jetPattern: JetPattern,
    rock: RockPattern,
    jetPatternOffset: Int,
  ): (Field2D[Square], Int) = {
    val minY                    = fieldMinY(field) // should perhaps cache
    val startPosition: Coords2D = Coords2D.of(2, minY - 4)
    val rockCoords              = rock.coords.map(_ + startPosition)

    val (restingCoords, newOffset) =
      Simulation.runWithIterationCount(rockCoords) {
        case (rockCoords, iteration) =>
          if (Debug) {
            println("The rock starts falling")
            printField(field, rockCoords)
          }

          val totalJetPatternOffset = jetPatternOffset + iteration
          if (Debug) {
            println(s"Offset in jet pattern: $totalJetPatternOffset")
          }
          val nextMove              = jetPattern(totalJetPatternOffset % jetPattern.length)

          val movedByJetStream = rockCoords.map(_ + nextMove.diff)
          val adjusted         = if (hitsFieldOrSides(field, movedByJetStream)) {
            if (Debug) {
              println(
                s"Jet of gas pushes rock ${nextMove.toString}, but nothing happens"
              )
              printField(field, rockCoords)
            }

            rockCoords
          } else {
            if (Debug) {
              println(s"Jet of gas pushes rock ${nextMove.toString}")
              printField(field, movedByJetStream)
            }

            movedByJetStream
          }

          val ifGoesDown = adjusted.map(_.S)
          if (hitsFieldOrSides(field, ifGoesDown)) {
            if (Debug) {
              println(s"Rock falls 1 unit, causing it to come to rest")
              printField(field, adjusted)
            }
            (adjusted, totalJetPatternOffset + 1).asLeft
          } else {
            if (Debug) {
              println(s"Rock falls 1 unit")
              printField(field, ifGoesDown)
            }
            ifGoesDown.asRight
          }
      }

    val result = restingCoords.foldLeft(field) { case (acc, c) =>
      if (!acc.isValidCoordinate(c)) {
        println("We ran out of space:")
        printField(acc)
      }
      acc.updatedAtUnsafe(c, Square.Wall)
    }

    (result, newOffset)
  }

  private val FieldWidth = 7
  def fieldWithBottom(height: Int): Field2D[Square] = {
    val emptyField: Field2D[Square] = Field2D.ofSize(FieldWidth, height, Square.Empty)
    emptyField.mapByCoords { c =>
      if (c.y.value == emptyField.height - 1) { // (c.x.value == 0) || (c.x.value == emptyField.width - 1)
        Square.Wall
      } else {
        Square.Empty
      }
    }
  }

  def part1(jetPattern: JetPattern, rocksToCount: Long): Long = {
    println(jetPattern.length)

    val withBottom = fieldWithBottom(10000)

    val result = Simulation.runWithIterationCount((withBottom, 0)) {
      case (state, iteration) =>
        val (acc, jetPatternOffset) = state
        if (iteration >= rocksToCount) {
          println(s"Finished with jet pattern offset $jetPatternOffset")
          acc.asLeft
        } else {
          val rockIdx = iteration % RockPattern.RockPatterns.length
          val chosenRock                    = RockPattern.RockPatterns(rockIdx)

          // println(s"Iteration $iteration has jet pattern idx ${jetPatternOffset % jetPattern.length} and rock index of ${rockIdx}")

          val (result, newJetPatternOffset) =
            dropRock(acc, jetPattern, chosenRock, jetPatternOffset)

          (result, newJetPatternOffset).asRight
        }
    }

    printField(result, Set.empty)

    // tower height
    result.height - fieldMinY(result) - 1
  }

  type FrontierIndex = Int
  private var nextFrontierIndex: Int = 0
  private val frontierMap1: mutable.Map[FrontierIndex, Set[Coords2D]] = mutable.Map.empty
  private val frontierMap2: mutable.Map[Set[Coords2D], FrontierIndex] = mutable.Map.empty

  final case class State(
    frontierIndex: FrontierIndex,
    rockOffset: Int,
    jetPatternOffset: Int,
  )

  private def transitionFunction(jetPattern: JetPattern, state: State): (State, Int) = {
    val thisRock = RockPattern.RockPatterns(state.rockOffset)

    val newRockOffset = (state.rockOffset + 1) % RockPattern.RockPatterns.length

    val field = calculateField(state.frontierIndex)
    val (resultingField, returnedJetPatternOffset) = dropRock(
      field,
      jetPattern,
      thisRock,
      state.jetPatternOffset,
    )

    val newJetPatternOffset = returnedJetPatternOffset % jetPattern.length
    val newFrontierIndex = calculateAndCacheFrontier(resultingField)
    val heightDiff = fieldMinY(field) - fieldMinY(resultingField)

    (
      State(
        newFrontierIndex,
        newRockOffset,
        newJetPatternOffset,
      ),
      heightDiff,
    )
  }

  private def calculateField(frontierIndex: FrontierIndex): Field2D[Square] = {
    calculateField(frontierMap1(frontierIndex))
  }

  val FieldHeightForExperiments = 100
  private def calculateField(frontier: Set[Coords2D]): Field2D[Square] = {
    val field: Field2D[Square] = Field2D.ofSize(FieldWidth, FieldHeightForExperiments, Square.Empty)
    val frontierMaxY = frontier.map(_.y.value).max
    val yDelta = field.height - frontierMaxY - 1

    val result = frontier.foldLeft(field) { case (acc, c) =>
      acc.updatedAtUnsafe(c + Coords2D.of(0, yDelta), Square.Wall)
    }

//    printField(result)
    result
  }

  def printFrontier(frontier: Set[Coords2D]): Unit = {
    printField(calculateField(frontier))
  }

  private def calculateFrontier(field: Field2D[Square]): Set[Coords2D] = {
    val minY = fieldMinY(field)
    val frontier: mutable.Set[Coords2D] = mutable.Set.empty

    (0 until field.width) foreach { x =>
      Bfs.bfsVisitAll[Coords2D](
        Coords2D.of(x, minY - 1),
        c => {
          if (field(c) == Square.Empty) {
            field.adjacent4(c).filter(_.y.value >= minY - 1)
          } else {
            Nil
          }
        },
        c => {
          if (field(c) != Square.Empty) {
            frontier.add(c)
          }
        }
      )
    }

    val result = frontier.toSet

    if (Debug) {
      println("Frontier for field")
      println("Field")
      printField(field)
      println("Frontier")
      printFrontier(result)
      println
    }

    result
  }

  private def calculateAndCacheFrontier(field: Field2D[Square]): FrontierIndex = {
    val frontier = calculateFrontier(field)
    frontierMap2.get(frontier) match {
      case Some(idx) =>
        idx

      case None =>
        val idx = nextFrontierIndex
        nextFrontierIndex += 1
        frontierMap1.update(idx, frontier)
        frontierMap2.update(frontier, idx)
        idx
    }
  }

  def part2(jetPattern: JetPattern, rocksToCount: Long, loopDetection: Boolean = true): Long = {
    val initialField = fieldWithBottom(FieldHeightForExperiments)
    val initialFrontier = calculateAndCacheFrontier(initialField)

    var state = State(initialFrontier, 0, 0)
    // State -> (move_num, growth_num)
    val visited: mutable.HashMap[State, (Long, Long)] = mutable.HashMap.empty
    visited.put(state, (0, 0))

    var acc: Long = 0
    var move: Long = 0
    while (move <= rocksToCount) {
      val (newState, growth) = transitionFunction(jetPattern, state)
      if (loopDetection && visited.contains(newState)) { // we have our loop
        move += 1
        val nowAt = acc + growth
        val (whenWeSawIt, howTallWasIt) = visited(newState)
        val loopSizeInMoves = move - whenWeSawIt
        val loopSizeInGrowth = nowAt - howTallWasIt
        val remaining = rocksToCount - move
        val loopsThatFit = remaining / loopSizeInMoves
        println(s"omg loop $newState after $move moves which we first saw in $whenWeSawIt (move delta $loopSizeInMoves, growth delta $loopSizeInGrowth)")
        // TODO: we end up not reusing much of this stuff
        // val newRocksToCount = rocksToCount % loopSizeInMoves // rocksToCount - (loopsThatFit * loopSizeInMoves)
        val newRocksToCount = rocksToCount - (loopsThatFit * loopSizeInMoves)

        println(s"New rocks to count = ${newRocksToCount}")
        println(s"Extras is ${loopsThatFit * loopSizeInGrowth}")
        return part2(jetPattern, newRocksToCount, loopDetection = false) + loopsThatFit * loopSizeInGrowth
      } else {
        acc += growth
        state = newState
        move += 1
        visited.put(state, (move, acc))
      }
    }

    acc
  }

  def main(args: Array[String]): Unit = {
    val testData = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""
    val realData = readFileText("2022/17.txt")

    val test = parse(testData)
    val real = parse(realData)

//    part1(test, 2022) shouldEqual 3068
//    part1(real, 2022) shouldEqual 3071

//      part1(test, 97) + 2915 shouldEqual 3068

//      part2(test, 2022) shouldEqual 3068
//      part2(test, 2022, loopDetection = false) shouldEqual 3068
//    part2(real, 2022) shouldEqual 3071
//    part2(test, 1000000000000L) shouldEqual 1514285714288L

    part1(test, 85) + 1514285714157L shouldEqual 1514285714288L

    val newRocks = 3445
    val extras = 1523615155101L

    part1(real, newRocks) + extras shouldEqual 1523615160362L
//    part2(real, 1000000000000L) shouldEqual "todo"


    // 1523615160353 too low
    // 1523615160354 too low

    // 1523615160362 try next

    // 1523615160363 incorrect
    // 1523615160364 incorrect
    // 1523615160365 too high
  }
}
