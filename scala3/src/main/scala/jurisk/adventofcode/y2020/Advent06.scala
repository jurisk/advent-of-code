package jurisk.adventofcode.y2020

import cats.implicits._
import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.MultiLineAdventApp

type Answer = Char
type Form   = Set[Answer]
type Group  = Set[Form]

type MergeFunction[T] = (Set[T], Set[T]) => Set[T]

object Advent06 extends MultiLineAdventApp[Group, Int, Int]:
  val year: Int     = 2020
  val exercise: Int = 6

  def solve(testCases: List[Group], f: MergeFunction[Answer]): Int =
    testCases.map(_.reduce(f).size).sum

  def solution1(testCases: List[Group]): Int = solve(testCases, _.union(_))
  def solution2(testCases: List[Group]): Int = solve(testCases, _.intersect(_))

  def parseLines(lines: List[String]): Either[ErrorMessage, Group] =
    lines.map(_.toSet).toSet.asRight
