package jurisk.adventofcode.y2018

import cats.implicits._
import jurisk.geometry.Direction2D._
import jurisk.geometry.Rotation._
import jurisk.geometry._
import jurisk.utils.FileInput._
import jurisk.utils.Simulation
import jurisk.utils.CollectionOps.IterableOps
import jurisk.utils.Parsing.StringOps
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

object Advent13 {
  type Result2 = String

  final case class Cart(
    direction: CardinalDirection2D,
    coords: Coords2D,
    turnsMade: Int,
  ) {
    override def toString: String =
      s"coords = $coords, direction = $direction, turnsMade = $turnsMade"

    private val rotations = List(Left90, NoRotation, Right90)

    def directionSymbol: Char = direction match {
      case N => '^'
      case E => '>'
      case W => '<'
      case S => 'v'
      case _ => '?'
    }

    def next(board: Field2D[Track]): Cart = {
      val square = board.atOrElse(coords, Track.Empty)

      val newDirection = square match {
        case Track.Intersection =>
          val chosenRotation = rotations(turnsMade % rotations.length)
          direction.rotate(chosenRotation)

        case Track.N_S   => direction
        case Track.E_W   => direction
        case Track.SW_NE =>
          direction match {
            case Direction2D.S => W
            case Direction2D.E => N
            case Direction2D.N => E
            case Direction2D.W => S
            case _             => s"Unexpected direction $direction".fail
          }
        case Track.NW_SE =>
          direction match {
            case Direction2D.S => E
            case Direction2D.E => S
            case Direction2D.N => W
            case Direction2D.W => N
            case _             => s"Unexpected direction $direction".fail
          }
        case Track.Empty => s"Unexpected situation $square".fail
      }

      Cart(
        newDirection,
        coords + newDirection,
        turnsMade + (if (square == Track.Intersection) 1 else 0),
      )
    }
  }

  sealed trait Track {
    def symbol: Char
  }
  case object Track  {
    case object Empty        extends Track {
      override def symbol: Char = ' '
    }
    case object N_S          extends Track {
      override def symbol: Char = '|'
    }
    case object E_W          extends Track {
      override def symbol: Char = '-'
    }
    case object SW_NE        extends Track {
      override def symbol: Char = '/'
    }
    case object NW_SE        extends Track {
      override def symbol: Char = '\\'
    }
    case object Intersection extends Track {
      override def symbol: Char = '+'
    }
  }

  sealed trait ProcessingStrategy
  object ProcessingStrategy {
    case object StopOnCollisionReturningLocation extends ProcessingStrategy
    case object StopOnSingleCartRemainingReturningLocation
        extends ProcessingStrategy
  }

  final case class State(
    processingStrategy: ProcessingStrategy,
    board: Field2D[Track],
    carts: Set[Cart],
  ) {
    override def toString: String = {
      val cartInfo = carts.map { cart =>
        s"$cart\n"
      }.mkString

      val field = Field2D.toDebugRepresentation(
        carts.foldLeft(board.map(_.symbol)) { case (acc, cart) =>
          acc.updatedAtUnsafe(cart.coords, cart.directionSymbol)
        }
      )

      s"$cartInfo\n$field\n\n"
    }

    private def moveCartDetectingCollision(
      cart: Cart,
      otherCarts: Set[Cart],
    ): Either[Coords2D, Cart] = {
      val newPosition    = cart.next(board)
      val collidingCarts =
        otherCarts.filter(_.coords == newPosition.coords)
      if (collidingCarts.nonEmpty) { // collision!
        newPosition.coords.asLeft
      } else {
        newPosition.asRight
      }
    }

    private def endOfTurnProcessing(
      acc: Set[Cart]
    ): Either[Coords2D, Set[Cart]] = acc.size match {
      case 0 => "We should not lose all carts".fail
      case 1 => acc.singleElementUnsafe.coords.asLeft
      case _ => acc.asRight
    }

    @tailrec
    private def moveAllCarts(
      queue: List[Cart],
      movedCarts: Set[Cart] = Set.empty,
    ): Either[Coords2D, Set[Cart]] =
      queue match {
        case Nil =>
          processingStrategy match {
            case ProcessingStrategy.StopOnCollisionReturningLocation           =>
              movedCarts.asRight
            case ProcessingStrategy.StopOnSingleCartRemainingReturningLocation =>
              endOfTurnProcessing(movedCarts)
          }

        case h :: t =>
          val allActiveCartsExceptH = movedCarts ++ t.toSet
          val result                = moveCartDetectingCollision(h, allActiveCartsExceptH)
          result match {
            case Left(collisionLocation) => // collision
              processingStrategy match {
                case ProcessingStrategy.StopOnCollisionReturningLocation =>
                  // return collision location and quit
                  collisionLocation.asLeft

                case ProcessingStrategy.StopOnSingleCartRemainingReturningLocation =>
                  // remove the collided carts and continue
                  moveAllCarts(
                    t.filter(_.coords != collisionLocation),
                    movedCarts.filter(_.coords != collisionLocation),
                  )
              }
            case Right(movedCart)        => moveAllCarts(t, movedCarts + movedCart)
          }
      }

    def next: Either[Coords2D, State] = {
      // Movement order is important!
      val cartsInMovementOrder =
        carts.toList.sortBy(cart => (cart.coords.y, cart.coords.x))
      val resultingCarts       = moveAllCarts(cartsInMovementOrder)
      resultingCarts map { resultingCarts => copy(carts = resultingCarts) }
    }
  }

  def parse(data: String, processingStrategy: ProcessingStrategy): State = {
    val field: Field2D[Char] = Field2D.parseCharField(data)
    val carts                = field.entries.flatMap { case (c, ch) =>
      Direction2D.parseCaretToOption(ch) map { d => Cart(d, c, 0) }
    }.toSet

    val board: Field2D[Track] = field.map {
      case ' '             => Track.Empty
      case '|' | 'v' | '^' => Track.N_S
      case '-' | '>' | '<' => Track.E_W
      case '+'             => Track.Intersection
      case '\\'            => Track.NW_SE
      case '/'             => Track.SW_NE
      case ch              => ch.toString.failedToParse
    }

    State(processingStrategy, board, carts)
  }

  def solve(state: State): Coords2D = Simulation.run(state)(_.next)

  def main(args: Array[String]): Unit = {
    val testData1 = """/->-\
                      ||   |  /----\
                      || /-+--+-\  |
                      || | |  | v  |
                      |\-+-/  \-+--/
                      |  \------/
                      |""".stripMargin
    val testData2 = """/>-<\
                      ||   |
                      || /<+-\
                      || | | v
                      |\>+</ |
                      |  |   ^
                      |  \<->/""".stripMargin
    val realData  = readFileText("2018/13.txt")

    val test1 =
      parse(testData1, ProcessingStrategy.StopOnCollisionReturningLocation)
    val test2 = parse(
      testData2,
      ProcessingStrategy.StopOnSingleCartRemainingReturningLocation,
    )
    val real1 =
      parse(realData, ProcessingStrategy.StopOnCollisionReturningLocation)
    val real2 = parse(
      realData,
      ProcessingStrategy.StopOnSingleCartRemainingReturningLocation,
    )

    solve(test1) shouldEqual Coords2D.of(7, 3)
    solve(real1) shouldEqual Coords2D.of(136, 36)
    solve(test2) shouldEqual Coords2D.of(6, 4)
    solve(real2) shouldEqual Coords2D.of(53, 111)
  }
}
