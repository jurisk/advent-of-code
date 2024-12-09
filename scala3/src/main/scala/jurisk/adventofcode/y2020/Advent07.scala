package jurisk.adventofcode.y2020

import cats.implicits.*
import AdventApp.ErrorMessage
import jurisk.adventofcode.y2020.Advent07.Bag

object Advent07 extends SingleLineAdventApp[Bag, Int]:
  opaque type Colour = String
  final case class Bag(colour: Colour, counts: Map[Colour, Int])
  
  def fileName: String = "07.txt"

  private val Target: Colour = new Colour("shiny gold")

  // a tree would be more suitable, but a `Map` will do
  private def toMap(testCases: List[Bag]): Map[Colour, Bag] =
    testCases.groupBy(_.colour).view.mapValues {
      case Nil => sys.error("Unexpectedly got empty list")
      case x :: Nil => x
      case xs => sys.error(s"Unexpectedly got more than one entry for colour: $xs")
    }.toMap
  
  def solution1(testCases: List[Bag]): Int =
    val map: Map[Colour, Bag] = toMap(testCases)

    def canContainTarget(bag: Bag): Boolean =
      bag.counts.contains(Target) || bag.counts.keySet.exists { (x: Colour) =>
        map.get(x).map(canContainTarget).getOrElse(false)
      }
    
    testCases.count(x => canContainTarget(x))
  
  def solution2(testCases: List[Bag]): Int =
    val map: Map[Colour, Bag] = toMap(testCases)

    def count(colour: Colour): Int =
      1 + map.get(colour).map { x =>
        x.counts.map { case (k, v) => count(k) * v }.sum
      }.getOrElse(0)
    
    count(Target) - 1 // subtracting 1 as we don't count our target bag

  private val OuterRegEx = """([\w ]+) bags contain (.*)\.""".r
  private val InnerRegExEmpty = """no other bags"""
  private val InnerRegExFull = """(\d+) ([\w ]+) bag[s]?""".r
  
  def parseLine(line: String): Either[ErrorMessage, Bag] =
    line match
      case OuterRegEx(bag, inner) =>
        val counts = inner match
          case InnerRegExEmpty => 
            List.empty.asRight
            
          case _ =>
            inner
              .split(", ")
              .toList
              .map { x =>
                x match {
                  case InnerRegExFull(count, colour) 
                    => (colour -> count.toInt).asRight
                  case _                         
                    => ErrorMessage(x).asLeft
                }
              }
              .sequence

        counts.map(x => Bag(bag, x.toMap))
        
      case _ => 
        ErrorMessage(line).asLeft
