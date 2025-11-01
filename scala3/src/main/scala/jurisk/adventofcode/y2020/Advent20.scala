package jurisk.adventofcode.y2020

import cats.implicits._
import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.MultiLineAdventApp
import jurisk.adventofcode.y2020.Advent20.TileData
import jurisk.adventofcode.y2020.Advent20.TileId

import scala.annotation.tailrec
import scala.math.sqrt

object Advent20 extends MultiLineAdventApp[(TileId, TileData), Long, Int]:
  override val year: TileId     = 2020
  override val exercise: TileId = 20

  opaque type TileId = Int

  opaque type Edge   = String
  private type Pixel = Char

  final case class TileData(lines: List[String]):
    override def toString: String = "\n" + lines.mkString("\n") + "\n"

    def width: Int  = lines.head.length
    def height: Int = lines.length

    // all these are clockwise and it's important to remember this!
    def top: Edge    = lines.head
    def bottom: Edge = lines.last.reverse
    def right: Edge  = lines.map(_.last).mkString
    def left: Edge   = lines.map(_.head).reverse.mkString

    private def flipVertically: TileData = TileData(lines.map(_.reverse))

    def allEdges: List[Edge] =
      val flipped = flipVertically
      List(
        top,
        right,
        bottom,
        left,
        flipped.top,
        flipped.right,
        flipped.bottom,
        flipped.left,
      )

    def orient(orientation: Orientation): TileData = orientation match
      case Orientation.N000 => this
      case Orientation.N090 => rot90
      case Orientation.N180 => rot90.rot90
      case Orientation.N270 => rot90.rot90.rot90
      case Orientation.F000 => flipVertically
      case Orientation.F090 => flipVertically.rot90
      case Orientation.F180 => flipVertically.rot90.rot90
      case Orientation.F270 => flipVertically.rot90.rot90.rot90

    private def rot90: TileData = TileData(
      lines.head.indices
        .map { col =>
          lines.indices.map { row =>
            lines(row)(col)
          }.mkString
        }
        .map(_.reverse)
        .toList
    )

    def snipEdges: TileData = TileData(
      lines.init.tail.map(_.init.tail)
    )

    def at(x: Int, y: Int): Pixel = lines(y)(x)

    def set(x: Int, y: Int, newCh: Pixel): TileData =
      TileData(
        lines.zipWithIndex.map { case (row, y2) =>
          row.zipWithIndex.map { case (ch, x2) =>
            if (x == x2) && (y == y2) then newCh else ch
          }.mkString
        }
      )

  enum Orientation:
    case N000
    case N090
    case N180
    case N270
    case F000
    case F090
    case F180
    case F270

  final case class OrientedTile(orientation: Orientation, tileId: TileId)

  private def findTopLeftCorner(tiles: Map[TileId, TileData]): OrientedTile =
    val allEdges = tiles.values.flatMap(_.allEdges)

    tiles
      .find { (_, tileData) =>
        val top  = tileData.top
        val left = tileData.left

        allEdges.count(_ == top) == 1 && allEdges.count(_ == left) == 1
      }
      .map { case (tileId, _) => OrientedTile(Orientation.N000, tileId) }
      .getOrElse(sys.error("Failed to find"))

  private def arrangeTiles(
    tiles: Map[TileId, TileData]
  ): List[List[OrientedTile]] =
    val topLeftCorner = findTopLeftCorner(tiles)
    val size: Int     = sqrt(tiles.size).round.toInt

    def allExceptMe(tile: OrientedTile): List[OrientedTile] = for
      orientation <- Orientation.values.toList
      tileId      <- tiles.keySet
      if tileId != tile.tileId // not me
    yield OrientedTile(orientation, tileId)

    def project(tile: OrientedTile): TileData =
      tiles(tile.tileId).orient(tile.orientation)

    def edgesMatchHorizontally(left: TileData, right: TileData): Boolean =
      left.right == right.left.reverse

    def edgesMatchVertically(top: TileData, bottom: TileData): Boolean =
      top.bottom == bottom.top.reverse

    def matchesHorizontally(left: OrientedTile, right: OrientedTile): Boolean =
      edgesMatchHorizontally(project(left), project(right))

    def matchesVertically(top: OrientedTile, bottom: OrientedTile): Boolean =
      edgesMatchVertically(project(top), project(bottom))

    def findMatchingTileHorizontally(tile: OrientedTile): OrientedTile =
      allExceptMe(tile)
        .find { x =>
          matchesHorizontally(tile, x)
        }
        .getOrElse(sys.error("Failed to find matching tile horizontally"))

    @tailrec
    def fillRowHorizontally(row: List[OrientedTile]): List[OrientedTile] =
      if row.size == size
      then row
      else
        fillRowHorizontally(row ++ List(findMatchingTileHorizontally(row.last)))

    def findMatchingTileVertically(tile: OrientedTile): OrientedTile =
      allExceptMe(tile)
        .find { x =>
          matchesVertically(tile, x)
        }
        .getOrElse(sys.error("Failed to find matching tile vertically"))

    def findMatchingRow(row: List[OrientedTile]): List[OrientedTile] =
      row map findMatchingTileVertically

    @tailrec
    def fillRowsVertically(
      rows: List[List[OrientedTile]]
    ): List[List[OrientedTile]] =
      if rows.size == size
      then rows
      else fillRowsVertically(rows ++ List(findMatchingRow(rows.last)))

    val topRow = fillRowHorizontally(topLeftCorner :: Nil)

    fillRowsVertically(List(topRow))

  def solve1(arrangement: List[List[OrientedTile]]): Long =
    List[Long](
      arrangement.head.head.tileId,
      arrangement.head.last.tileId,
      arrangement.last.head.tileId,
      arrangement.last.last.tileId,
    ).product

  private def mergeArrangement(
    map: Map[TileId, TileData],
    arrangement: List[List[OrientedTile]],
  ): TileData =
    val tileDatas: List[List[TileData]] = arrangement.map { row =>
      row map { tile =>
        map(tile.tileId).orient(tile.orientation).snipEdges
      }
    }

    def mergeRow(row: List[TileData]): List[String] =
      def mergeHorizontally(
        left: List[String],
        right: List[String],
      ): List[String] =
        (left zip right).map((a, b) => a + b)

      row.map(_.lines).reduce((l, r) => mergeHorizontally(l, r))

    val result = tileDatas.flatMap(mergeRow)

    TileData(result)

  private def subtractMonsterEverywhereAndCountHashes(
    monster: TileData,
    merged: TileData,
  ): Int =
    val monsterPositions = for
      x <- 0 until (merged.width - monster.width)
      y <- 0 until (merged.height - monster.height)
    yield (x, y)

    def subtractMonsterIfThere(data: TileData, x: Int, y: Int): TileData =
      val pixels = for
        xm <- 0 until monster.width
        ym <- 0 until monster.height
        if monster.at(xm, ym) == '#'
      yield data.at(x + xm, y + ym)

      if pixels.forall(_ == '#')
      then
        val monsterPixelPositions = for
          xm <- 0 until monster.width
          ym <- 0 until monster.height
          if monster.at(xm, ym) == '#'
        yield (xm, ym)

        monsterPixelPositions
          .foldLeft(data) { case (acc, (xm, ym)) =>
            acc.set(x + xm, y + ym, 'O')
          }
      else data

    val withoutMonsters = monsterPositions.foldLeft(merged) {
      case (acc, (x, y)) =>
        subtractMonsterIfThere(acc, x, y)
    }

    withoutMonsters.lines.map(_.count(_ == '#')).sum

  def solve2(monster: TileData, merged: TileData): Int =
    val monsters: List[TileData] = Orientation.values.toList map monster.orient
    assert(monsters.distinct == monsters)
    monsters.map(m => subtractMonsterEverywhereAndCountHashes(m, merged)).min

  private val monster = TileData(
    List(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   ",
    )
  )

  private val TileIdRe = """Tile (\d+):""".r

  override def parseLines(
    lines: List[String]
  ): Either[ErrorMessage, (TileId, TileData)] =
    lines match
      case Nil     => ErrorMessage("Empty lines").asLeft
      case x :: xs =>
        val tileId: Int = x match
          case TileIdRe(id) => id.toInt
          case _            => sys.error("Failed to match $x")
        (tileId, TileData(xs)).asRight

  override def solution1(input: List[(TileId, TileData)]): Long = {
    val tiles    = input.toMap
    val arranged = arrangeTiles(input.toMap)
    solve1(arranged)
  }

  override def solution2(input: List[(TileId, TileData)]): TileId = {
    val tiles    = input.toMap
    val arranged = arrangeTiles(input.toMap)
    val merged   = mergeArrangement(tiles, arranged)
    solve2(monster, merged)
  }
