package jurisk.adventofcode.y2018

import org.scalatest.flatspec.AnyFlatSpec
import Advent15._
import cats.implicits._
import jurisk.geometry.Coords2D
import org.scalatest.matchers.should.Matchers._

class Advent15Spec extends AnyFlatSpec {
  "Advent 15" should "transition test 1 round 23 to round 24" in {
    val test1After23Rounds = parse("""#######
                                     |#...G.#
                                     |#..G.G#
                                     |#.#.#G#
                                     |#...#E#
                                     |#.....#
                                     |#######
                                     |""".stripMargin)
      .copy(
        roundsCompleted = 23
      )
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 2)), 131)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 3)), 131)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 4)), 131)

    println("Test 1 After 23 rounds input:")
    test1After23Rounds.debugPrint()

    val test1After24RoundsExpected = parse("""#######
                                             |#..G..#
                                             |#...G.#
                                             |#.#G#G#
                                             |#...#E#
                                             |#.....#
                                             |#######""".stripMargin)
      .copy(
        roundsCompleted = 24
      )
      .setUnitHitPoints(WarriorId(Coords2D.of(4, 2)), 131)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 3)), 128)
      .setUnitHitPoints(WarriorId(Coords2D.of(5, 4)), 128)
    println("Test 1 After 24 rounds expected:")
    test1After24RoundsExpected.debugPrint()

    val test1After24RoundsObtained = test1After23Rounds.nextRound.get
    println("Test 1 After 24 rounds obtained:")
    test1After24RoundsObtained.debugPrint()

    test1After24RoundsObtained.debugRepresentation shouldEqual test1After24RoundsExpected.debugRepresentation
  }

  private def runTestAgainstExpected(
    test: String,
    outcome: Int,
    rounds: Int,
    expected: String,
  ): Unit = {
    val parsed                 = parse(test)
    val (finishState, result)  = part1(parsed)
    val obtainedRepresentation = finishState.debugRepresentation
    obtainedRepresentation shouldEqual expected.replace("\r\n", "\n")
    finishState.roundsCompleted shouldEqual rounds
    result shouldEqual outcome
  }

  it should "work test 1" in {
    val testData1 =
      """#######
        |#.G...#
        |#...EG#
        |#.#.#G#
        |#..G#E#
        |#.....#
        |#######""".stripMargin

    val expected = """#######
                     |#G....#   G(200)
                     |#.G...#   G(131)
                     |#.#.#G#   G(59)
                     |#...#.#
                     |#....G#   G(200)
                     |#######""".stripMargin

    runTestAgainstExpected(testData1, 27730, 47, expected)
  }

  private def splitCombatSummary(data: String): (String, String) = {
    val lines = data.split("\\R").toList
    val width = lines.head.takeWhile(_ == '#').length

    val (a, b) = lines.map(s => (s.take(width), s.drop(width + 7))).unzip
    (a.mkString("\n"), b.mkString("\n"))
  }

  private def runTest(data: String, outcome: Int, rounds: Int): Unit = {
    val (input, expected) = splitCombatSummary(data)
    runTestAgainstExpected(input, outcome, rounds: Int, expected)
  }

  it should "work in test 2" in {
    val testData =
      """#######       #######
        |#G..#E#       #...#E#   E(200)
        |#E#E.E#       #E#...#   E(197)
        |#G.##.#  -->  #.E##.#   E(185)
        |#...#E#       #E..#E#   E(200), E(200)
        |#...E.#       #.....#
        |#######       #######""".stripMargin

    runTest(testData, 36334, 37)
  }

  it should "work in test 3" in {
    val testData3 =
      """#######       #######
        |#E..EG#       #.E.E.#   E(164), E(197)
        |#.#G.E#       #.#E..#   E(200)
        |#E.##E#  -->  #E.##.#   E(98)
        |#G..#.#       #.E.#.#   E(200)
        |#..E#.#       #...#.#
        |#######       #######""".stripMargin

    runTest(testData3, 39514, 46)
  }

  it should "work in test 4" in {

    val testData4 =
      """#######       #######
        |#E.G#.#       #G.G#.#   G(200), G(98)
        |#.#G..#       #.#G..#   G(200)
        |#G.#.G#  -->  #..#..#
        |#G..#.#       #...#G#   G(95)
        |#...E.#       #...G.#   G(200)
        |#######       #######""".stripMargin
    runTest(testData4, 27755, 35)
  }

  it should "work in test 5" in {

    val testData5 =
      """#######       #######
        |#.E...#       #.....#
        |#.#..G#       #.#G..#   G(200)
        |#.###.#  -->  #.###.#
        |#E#G#G#       #.#.#.#
        |#...#G#       #G.G#G#   G(98), G(38), G(200)
        |#######       #######""".stripMargin

    runTest(testData5, 28944, 54)
  }

  it should "work in test 6" in {
    val testData6 =
      """#########       #########
        |#G......#       #.G.....#   G(137)
        |#.E.#...#       #G.G#...#   G(200), G(200)
        |#..##..G#       #.G##...#   G(200)
        |#...##..#  -->  #...##..#
        |#...#...#       #.G.#...#   G(200)
        |#.G...G.#       #.......#
        |#.....G.#       #.......#
        |#########       #########""".stripMargin

    runTest(testData6, 18740, 20)
  }
}
