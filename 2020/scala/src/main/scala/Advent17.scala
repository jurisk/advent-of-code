import Advent17.Coordinates.{Coordinates3D, Coordinates4D}
import AdventApp.ErrorMessage
import cats.implicits._

object Advent17 extends SingleLineAdventApp[List[Boolean], Int]:
  sealed trait Coordinates[C <: Coordinates[C]]:
    infix def to(other: C): Seq[C]

    def neighbours: Seq[C] = (this.minusOne to this.plusOne).filterNot(_ == this)
    
    def plusOne: C = this + One
    def minusOne: C = this - One

    def -(other: C): C = biProject(other, (a, b) => a - b)
    def +(other: C): C = biProject(other, (a, b) => a + b)

    def One: C

    def biProject(other: C, f: (Int, Int) => Int): C

    infix def lower(other: C): C = biProject(other, (a, b) => Math.min(a, b))
    infix def upper(other: C): C = biProject(other, (a, b) => Math.max(a, b))
  
  object Coordinates:
    object Coordinates3D:
      def fromXY(x: Int, y: Int): Coordinates3D = Coordinates3D(x, y, 0)
    
    final case class Coordinates3D(x: Int, y: Int, z: Int) extends Coordinates[Coordinates3D]:
      infix def to(other: Coordinates3D): Seq[Coordinates3D] =
        for
          x <- x to other.x
          y <- y to other.y
          z <- z to other.z
        yield Coordinates3D(x, y, z)

      def One = Coordinates3D(1, 1, 1)

      def biProject(other: Coordinates3D, f: (Int, Int) => Int): Coordinates3D =
        Coordinates3D(
          f(x, other.x),
          f(y, other.y),
          f(z, other.z),
        )

    object Coordinates4D:
      def fromXY(x: Int, y: Int): Coordinates4D = Coordinates4D(x, y, 0, 0)
    
    final case class Coordinates4D(x: Int, y: Int, z: Int, w: Int) extends Coordinates[Coordinates4D]:
      infix def to(other: Coordinates4D): Seq[Coordinates4D] =
        for
          x <- x to other.x
          y <- y to other.y
          z <- z to other.z
          w <- w to other.w
        yield Coordinates4D(x, y, z, w)

      def One = Coordinates4D(1, 1, 1, 1)
      
      def biProject(other: Coordinates4D, f: (Int, Int) => Int): Coordinates4D =
        Coordinates4D(
          f(x, other.x),
          f(y, other.y),
          f(z, other.z),
          f(w, other.w),
        )
  
  final case class Grid[C <: Coordinates[C]](data: Map[C, Boolean]):
    def at(coordinates: C): Boolean = data.get(coordinates).getOrElse(false)
    def set(coordinates: C, value: Boolean) = copy(data = data + (coordinates -> value))
      
    def countActive: Int = data.count { case (_, value) => value }
    
    def coordinatesSet: Set[C] = data.keySet
    
    def lowerCoordinates: C = data.keySet.reduce(_ lower _)
    def upperCoordinates: C = data.keySet.reduce(_ upper _)
    
    def neighbourCount(coordinates: C): Int =
      coordinates.neighbours.count(at)
    
    def nextValue(coordinates: C): Boolean =
      val neighboursActive = neighbourCount(coordinates)

      (at(coordinates), neighboursActive) match
        case (_, 3) => true
        case (true, 2) => true
        case _ => false
    
    def nextIteration: Grid[C] =
      val newData = (lowerCoordinates.minusOne to upperCoordinates.plusOne)
        .map(coordinates => coordinates -> nextValue(coordinates))
      
      Grid(newData.toMap)

  object Grid:
    def empty[C <: Coordinates[C]]: Grid[C] = new Grid(Map.empty)
  
  def fileName: String = "17.txt"

  def solve[C <: Coordinates[C]](grid: Grid[C], iterations: Int): Int =
    val resultingGrid = (0 until iterations)
      .foldLeft(grid) { case (acc, iteration) =>
        acc.nextIteration 
      }
    
    resultingGrid.countActive
  
  def solution1(input: List[List[Boolean]]): Int =
    val grid: Grid[Coordinates3D] = convertInput(input, Coordinates.Coordinates3D.fromXY)
    solve(grid, 6)

  def solution2(input: List[List[Boolean]]): Int =
    val grid: Grid[Coordinates4D] = convertInput(input, Coordinates.Coordinates4D.fromXY)
    solve(grid, 6)

  def parseLine(line: String): Either[ErrorMessage, List[Boolean]] =
    line
      .map {
        case '.' => false.asRight
        case '#' => true.asRight
        case x   => ErrorMessage(s"Unrecognized $x").asLeft
      }
      .toList
      .sequence

  def convertInput[C <: Coordinates[C]](data: List[List[Boolean]], fromXY: (Int, Int) => C): Grid[C] =
    data.zipWithIndex
      .flatMap { case (y, yIdx) =>
        y.zipWithIndex
          .map { case (value, xIdx) =>
            fromXY(xIdx, yIdx) -> value
          }
      }
      .foldLeft(Grid.empty[C]) { case (acc, (coordinates, value)) =>
        acc.set(coordinates, value)
      }
