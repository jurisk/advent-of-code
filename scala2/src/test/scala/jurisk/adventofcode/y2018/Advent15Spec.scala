package jurisk.adventofcode.y2018

import jurisk.adventofcode.y2018.Advent15.Race.Elf
import jurisk.geometry.Coords2D
import jurisk.utils.FileInput.readFileText
import jurisk.utils.Parsing.StringOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import Advent15._

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

    val test1After24RoundsObtained =
      test1After23Rounds.nextRound.getOrElse("Failed".fail)
    println("Test 1 After 24 rounds obtained:")
    test1After24RoundsObtained.debugPrint()

    test1After24RoundsObtained.debugRepresentation shouldEqual test1After24RoundsExpected.debugRepresentation
  }

  private def runTestAgainstExpected(
    test: String,
    outcome: Int,
    rounds: Int,
    expected: String,
    elvenAttackPower: Int,
  ): Unit = {
    val parsed                 = parse(test).updateAttackPower(Elf, elvenAttackPower)
    val (finishState, result)  = part1(parsed)
    val obtainedRepresentation = finishState.debugRepresentation
    obtainedRepresentation shouldEqual expected.replace("\r\n", "\n")
    finishState.roundsCompleted shouldEqual rounds
    result shouldEqual outcome
  }

  private def splitCombatSummary(data: String): (String, String) = {
    val lines = data.split("\\R").toList
    val width = lines.head.takeWhile(_ == '#').length

    val (a, b) = lines.map(s => (s.take(width), s.drop(width + 7))).unzip
    (a.mkString("\n"), b.mkString("\n"))
  }

  private def runTestPart1(data: String, outcome: Int, rounds: Int): Unit = {
    val (input, expected) = splitCombatSummary(data)
    runTestAgainstExpected(input, outcome, rounds, expected, 3)
  }

  private def runTestPart2(
    data: String,
    outcome: Int,
    rounds: Int,
    elvenAttackPower: Int,
  ): Unit = {
    val (input, expected) = splitCombatSummary(data)
    runTestAgainstExpected(input, outcome, rounds, expected, elvenAttackPower)
  }

  it should "part 1 test 1" in { // Not sure why this fails when everything else succeeds
    val testData1 =
      """#######       #######
        |#.G...#       #G....#   G(200)
        |#...EG#       #.G...#   G(131)
        |#.#.#G#       #.#.#G#   G(59)
        |#..G#E#       #...#.#
        |#.....#       #....G#   G(200)
        |#######       #######""".stripMargin

    runTestPart1(testData1, 27730, 47)
  }

  it should "part 1 in test 2" in {
    val testData2 =
      """#######       #######
        |#G..#E#       #...#E#   E(200)
        |#E#E.E#       #E#...#   E(197)
        |#G.##.#  -->  #.E##.#   E(185)
        |#...#E#       #E..#E#   E(200), E(200)
        |#...E.#       #.....#
        |#######       #######""".stripMargin

    runTestPart1(testData2, 36334, 37)
  }

  it should "part 1 in test 3" in {
    val testData3 =
      """#######       #######
        |#E..EG#       #.E.E.#   E(164), E(197)
        |#.#G.E#       #.#E..#   E(200)
        |#E.##E#  -->  #E.##.#   E(98)
        |#G..#.#       #.E.#.#   E(200)
        |#..E#.#       #...#.#
        |#######       #######""".stripMargin

    runTestPart1(testData3, 39514, 46)
  }

  it should "part 1 in test 4" in {
    val testData4 =
      """#######       #######
        |#E.G#.#       #G.G#.#   G(200), G(98)
        |#.#G..#       #.#G..#   G(200)
        |#G.#.G#  -->  #..#..#
        |#G..#.#       #...#G#   G(95)
        |#...E.#       #...G.#   G(200)
        |#######       #######""".stripMargin
    runTestPart1(testData4, 27755, 35)
  }

  it should "part 1 in test 5" in {
    val testData5 =
      """#######       #######
        |#.E...#       #.....#
        |#.#..G#       #.#G..#   G(200)
        |#.###.#  -->  #.###.#
        |#E#G#G#       #.#.#.#
        |#...#G#       #G.G#G#   G(98), G(38), G(200)
        |#######       #######""".stripMargin

    runTestPart1(testData5, 28944, 54)
  }

  it should "part 1 in test 6" in {
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

    runTestPart1(testData6, 18740, 20)
  }

  it should "part 1 for real" ignore {
    val realData = readFileText("2018/15.txt")
    val real     = parse(realData)
    part1(real)._2 shouldEqual 243390
  }

  it should "part 2 in test 1" in {
    val testData1 =
      """#######       #######
        |#.G...#       #..E..#   E(158)
        |#...EG#       #...E.#   E(14)
        |#.#.#G#  -->  #.#.#.#
        |#..G#E#       #...#.#
        |#.....#       #.....#
        |#######       #######""".stripMargin

    runTestPart2(testData1, 4988, 29, 15)
  }

  it should "part 2 in test 2" in {
    val testData =
      """#######       #######
        |#E..EG#       #.E.E.#   E(200), E(23)
        |#.#G.E#       #.#E..#   E(200)
        |#E.##E#  -->  #E.##E#   E(125), E(200)
        |#G..#.#       #.E.#.#   E(200)
        |#..E#.#       #...#.#
        |#######       #######""".stripMargin

    runTestPart2(testData, 31284, 33, 4)
  }

  it should "part 2 in test 3" in {
    val testData =
      """#######       #######
        |#E.G#.#       #.E.#.#   E(8)
        |#.#G..#       #.#E..#   E(86)
        |#G.#.G#  -->  #..#..#
        |#G..#.#       #...#.#
        |#...E.#       #.....#
        |#######       #######""".stripMargin

    runTestPart2(testData, 3478, 37, 15)
  }

  it should "part 2 in test 4" in {
    val testData =
      """#######       #######
        |#.E...#       #...E.#   E(14)
        |#.#..G#       #.#..E#   E(152)
        |#.###.#  -->  #.###.#
        |#E#G#G#       #.#.#.#
        |#...#G#       #...#.#
        |#######       #######""".stripMargin

    runTestPart2(testData, 6474, 39, 12)
  }

  it should "part 2 in test 5" in {
    val testData =
      """#########       #########
        |#G......#       #.......#
        |#.E.#...#       #.E.#...#   E(38)
        |#..##..G#       #..##...#
        |#...##..#  -->  #...##..#
        |#...#...#       #...#...#
        |#.G...G.#       #.......#
        |#.....G.#       #.......#
        |#########       #########""".stripMargin

    runTestPart2(testData, 1140, 30, 34)
  }

  it should "part 2 for real" ignore {
    val realData = readFileText("2018/15.txt")
    val real     = parse(realData)
    val answer   = 59886
    part2(real) shouldEqual answer
  }
}
