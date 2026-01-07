package jurisk.adventofcode.y2025

import cats.Parallel
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.syntax.all._
import jurisk.algorithms.Backtracker
import jurisk.algorithms.Backtracking
import jurisk.geometry.Area2D
import jurisk.geometry.Coords2D
import jurisk.geometry.Field2D
import jurisk.geometry.Rotation
import jurisk.utils.FileInput._
import jurisk.utils.Parsing.StringOps
import jurisk.utils.Timing.timedWithResult

import scala.collection.immutable.ArraySeq

object Advent12 extends IOApp {
  private type Shape      = Field2D[Boolean]
  final case class ShapeId(id: Int)
  private type ShapeCount = Int

  final case class ShapeCounts(data: Map[ShapeId, ShapeCount]) {
    def totalCount: ShapeCount              = data.values.sum
    def allPlaced: Boolean                  = data.values.forall(_ == 0)
    def firstRemaining: Option[ShapeId]     = data.collectFirst {
      case (id, count) if count > 0 => id
    }
    def decrement(id: ShapeId): ShapeCounts = ShapeCounts(
      data.updated(id, data(id) - 1)
    )
    def remainingDescription: String        = data.filter(_._2 > 0).toString
  }

  private object ShapeCounts {
    def apply(map: Map[ShapeId, ShapeCount]): ShapeCounts = new ShapeCounts(map)
  }

  // Normalized shape cells (coordinates relative to top-left at origin)
  private type NormalizedCells = Set[Coords2D]

  // Convert a shape to its normalized cell coordinates
  private def normalizedCells(shape: Shape): NormalizedCells = {
    val cells = shape.filterCoordsByValue(true).toSet
    if (cells.isEmpty) Set.empty
    else {
      val minX = cells.map(_.x).min
      val minY = cells.map(_.y).min
      cells.map(c => Coords2D(c.x - minX, c.y - minY))
    }
  }

  // Generate all unique variants of a shape (rotations × flips)
  private def allVariants(shape: Shape): Set[NormalizedCells] = {
    val rotations = Seq(
      Rotation.NoRotation,
      Rotation.Right90,
      Rotation.TurnAround,
      Rotation.Left90,
    )

    val variants = for {
      rotation <- rotations
      flipped  <- Seq(false, true)
    } yield {
      val rotated = shape.rotate(rotation)
      val result  = if (flipped) rotated.reverseColumns else rotated
      normalizedCells(result)
    }

    variants.toSet
  }

  // Packing problem definition for backtracking
  final private case class PackingProblem(
    region: Area2D[Int],
    allShapeVariants: Map[ShapeId, Set[NormalizedCells]],
    shapeCounts: ShapeCounts,
  )

  // State during backtracking search
  final case class PackingState(
    occupied: Set[Coords2D],
    placedShapes: Vector[Set[Coords2D]], // For visualization only
    remaining: ShapeCounts,
  )

  // Labels for placed shapes: A-Z, a-z, 0-9, symbols - wraps around using modulus
  private val PlacementLabels: IndexedSeq[Char] =
    ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ "!@#$%^&*()+=[]{}|;:,.<>?"
  private val EmptyCell                         = '░'

  private def labelForIndex(idx: Int): Char =
    PlacementLabels(idx % PlacementLabels.size)

  private def formatGrid(
    region: Area2D[Int],
    placedShapes: Vector[Set[Coords2D]],
  ): String = {
    val coordToLabel: Map[Coords2D, Char] = placedShapes.zipWithIndex.flatMap {
      case (cells, idx) =>
        cells.map(_ -> labelForIndex(idx))
    }.toMap

    val sb = new StringBuilder
    sb.append("┌")
    sb.append("─" * region.width)
    sb.append("┐\n")

    for (y <- region.topLeft.y until region.topLeft.y + region.height) {
      sb.append("│")
      for (x <- region.topLeft.x until region.topLeft.x + region.width) {
        val coord = Coords2D(x, y)
        val char  = coordToLabel.getOrElse(coord, EmptyCell)
        sb.append(char)
      }
      sb.append("│\n")
    }

    sb.append("└")
    sb.append("─" * region.width)
    sb.append("┘")
    sb.toString()
  }

  // Backtracking implementation for shape packing
  private object ShapePacking
      extends Backtracking[PackingProblem, PackingState] {
    private val Debug = false

    override def root(p: PackingProblem): PackingState =
      PackingState(Set.empty, Vector.empty, p.shapeCounts)

    override def reject(p: PackingProblem, c: PackingState): Boolean =
      false // Rejection happens naturally when extensions returns empty

    override def accept(p: PackingProblem, c: PackingState): Boolean =
      c.remaining.allPlaced

    override def visit(p: PackingProblem, c: PackingState): Unit =
      if (Debug) {
        val totalPlaced = p.shapeCounts.totalCount - c.remaining.totalCount
        println(
          s"\nVisiting state: placed $totalPlaced shapes, remaining: ${c.remaining.remainingDescription}"
        )
        println(formatGrid(p.region, c.placedShapes))
      }

    private def isValidPlacement(
      placed: Set[Coords2D],
      region: Area2D[Int],
      occupied: Set[Coords2D],
    ): Boolean =
      placed.forall(region.contains) && !placed.exists(occupied.contains)

    // Score placement by how many neighbors are outside region or already occupied
    // Higher score = more "snug" fit = more promising
    private def placementScore(
      placed: Set[Coords2D],
      region: Area2D[Int],
      occupied: Set[Coords2D],
    ): Int = {
      val allNeighbors      = placed.flatMap(_.adjacent4)
      val externalNeighbors = allNeighbors.filterNot(region.contains)
      val occupiedNeighbors = allNeighbors.intersect(occupied)
      externalNeighbors.size + occupiedNeighbors.size
    }

    override def extensions(
      p: PackingProblem,
      c: PackingState,
    ): Seq[PackingState] =
      // Pick the first shape with remaining count > 0, try all its variants at all positions
      c.remaining.firstRemaining match {
        case None          => Nil
        case Some(shapeId) =>
          val validPlacements = for {
            variant  <- p.allShapeVariants(shapeId).toSeq
            position <- p.region.points
            placed    = variant.map(cell =>
                          Coords2D(cell.x + position.x, cell.y + position.y)
                        )
            if isValidPlacement(placed, p.region, c.occupied)
          } yield placed

          validPlacements
            .map(placed =>
              (placed, placementScore(placed, p.region, c.occupied))
            )
            .filter { case (_, score) => score > 0 }
            .sortBy { case (_, score) => -score }
            .map { case (placed, _) =>
              PackingState(
                c.occupied ++ placed,
                c.placedShapes :+ placed,
                c.remaining.decrement(shapeId),
              )
            }
      }
  }

  private def areaCheck(
    region: Area2D[Int],
    shapes: Map[ShapeId, Shape],
    shapeCounts: ShapeCounts,
  ): Option[String] = {
    val regionArea     = region.width * region.height
    val totalShapeArea = shapeCounts.data.map { case (id, count) =>
      shapes(id).filterCoordsByValue(true).size * count
    }.sum

    if (totalShapeArea > regionArea)
      Some(s"shapes too large (shapes=$totalShapeArea, region=$regionArea)")
    else
      None
  }

  final case class Region(size: Area2D[Int], shapeCounts: ShapeCounts) {
    def isValid(shapes: Map[ShapeId, Shape]): Either[String, PackingState] = {
      areaCheck(size, shapes, shapeCounts) match {
        case Some(reason) => return Left(reason)
        case None         =>
      }

      val allShapeVariants = shapes.map { case (id, shape) =>
        id -> allVariants(shape)
      }

      val problem = PackingProblem(
        region = size,
        allShapeVariants = allShapeVariants,
        shapeCounts = shapeCounts,
      )

      Backtracker.solve(ShapePacking)(problem) match {
        case Some(state) => Right(state)
        case None        => Left("no valid packing")
      }
    }
  }

  final case class Input(
    shapes: Map[ShapeId, Shape],
    regions: ArraySeq[Region],
  ) {
    def validRegionCountF[F[_]: Sync: Parallel]: F[ShapeCount] = {
      val total = regions.size

      regions.zipWithIndex.toList
        .parTraverse { case (region, idx) =>
          Sync[F].delay {
            val countsStr = region.shapeCounts.data.toSeq
              .sortBy(_._1.id)
              .map(_._2)
              .mkString(",")
            val label     =
              s"Region ${idx + 1}/$total ${region.size.width}x${region.size.height} [$countsStr]"

            val result = timedWithResult(
              label,
              (r: Either[String, PackingState]) =>
                r.fold(reason => s"INVALID - $reason", _ => "VALID"),
            ) {
              region.isValid(shapes)
            }

            result match {
              case Right(state) =>
                println(formatGrid(region.size, state.placedShapes))
                1
              case Left(_)      =>
                0
            }
          }
        }
        .map(_.sum)
    }
  }

  def parse(input: String): Input = {
    val sections = input.splitByDoubleNewline

    // Parse shapes from all sections except the last
    val shapes = ArraySeq
      .from(sections.init.map { section =>
        val lines     = section.splitLines
        val gridLines = lines.tail.mkString("\n")
        Field2D.parseBooleanField(gridLines)
      })
      .zipWithIndex
      .map { case (shape, index) =>
        ShapeId(index) -> shape
      }
      .toMap

    // Parse regions from the last section
    val regions =
      ArraySeq.from(sections.last.splitLines.filter(_.nonEmpty).map { line =>
        val (dims, counts)  = line.splitPairUnsafe(':')
        val (width, height) = dims.parsePairUnsafe('x', _.toInt, _.toInt)
        val shapeCounts     =
          ShapeCounts(counts.extractIntArraySeq.zipWithIndex.map {
            case (count, index) => ShapeId(index) -> count
          }.toMap)
        Region(
          Area2D.fromLeftTopWidthHeight(0, 0, width, height),
          shapeCounts,
        )
      })

    Input(shapes, regions)
  }

  def part1[F[_]: Sync: Parallel](data: Input): F[ShapeCount] =
    data.validRegionCountF

  def parseFile(fileName: String): Input =
    parse(readFileText(fileName))

  def fileName(suffix: String): String =
    s"2025/12$suffix.txt"

  override def run(args: List[String]): IO[ExitCode] = {
    val realData: Input = parseFile(fileName(""))

    for {
      p1 <- part1[IO](realData)
      _  <- IO.println(s"Part 1: $p1")
    } yield ExitCode.Success
  }
}
