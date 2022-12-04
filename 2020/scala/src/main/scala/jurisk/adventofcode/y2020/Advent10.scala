package jurisk.adventofcode.y2020

import cats.implicits.*
import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.SingleLineAdventApp

import scala.annotation.tailrec

object Advent10 extends SingleLineAdventApp[Int, Long]:
  def fileName: String = "10.txt"
  
  case class Solution(d1: Int = 0, d2: Int = 0, d3: Int = 0)
  
  def solution1(testCases: List[Int]): Long =
    val sorted = (0 :: (testCases.max + 3) :: testCases).sorted
    val diffs = (sorted.init zip sorted.tail).map { case (a, b) => b - a }
    
    val solution = diffs.foldLeft(Solution()) { (acc, x) =>
      x match
        case 1 => acc.copy(d1 = acc.d1 + 1)
        case 2 => acc.copy(d2 = acc.d2 + 1)
        case 3 => acc.copy(d3 = acc.d3 + 1)
        case _ => sys.error(s"Unexpected diff $x")
    }
    
    solution.d1 * solution.d3
  
  private def memoize[I, O](f: I => O): I => O = 
    new scala.collection.mutable.HashMap[I, O]() { self =>
      override def apply(key: I): O = self.synchronized(getOrElseUpdate(key, f(key)))
    }

  def solution2(testCases: List[Int]): Long =
    val sorted = testCases.sorted.toVector

    lazy val m: ((Int, Int)) => Long = memoize[(Int, Int), Long](f)
    
    def f: ((Int, Int)) => Long = { case (voltage, pointer) =>
      val x = sorted(pointer)
      if pointer == sorted.length - 1 
      then
        if (x - voltage > 3) 0 else 1
      else
          if (x - voltage > 3)
            0
          else
            m((x, pointer + 1)) + m((voltage, pointer + 1))
    }

    m((0, 0))

  def parseLine(line: String): Either[ErrorMessage, Int] =
    line.toIntOption.toRight(ErrorMessage(s"Failed to parse $line"))
