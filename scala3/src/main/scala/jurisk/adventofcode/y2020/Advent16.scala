package jurisk.adventofcode.y2020

import cats.implicits._
import jurisk.adventofcode.AdventApp
import jurisk.adventofcode.AdventApp.ErrorMessage
import jurisk.adventofcode.y2020.Advent16.Data

import scala.annotation.tailrec
import scala.util.matching.Regex

object Advent16 extends AdventApp[Data, Int, Long]:
  override val year: Value     = 2020
  override val exercise: Value = 16

  opaque type Name  = String
  opaque type Index = Int
  opaque type Value = Int

  final case class Field(name: Name, constraints: Set[Interval]):
    def isValid(n: Value): Boolean = constraints.exists(_.contains(n))

  final case class Interval(from: Value, to: Value):
    def contains(n: Value): Boolean = n >= from && n <= to

  final case class Ticket(numbers: Vector[Value]):
    def fieldValue(index: Index): Value = numbers(index)

    def invalidNumbers(fields: Set[Field]): Vector[Value] =
      numbers.filterNot(x => fields.exists(_.isValid(x)))

    def isValid(fields: Set[Field]): Boolean = invalidNumbers(fields).isEmpty

  final case class Data(
    fields: Set[Field],
    yourTicket: Ticket,
    nearbyTickets: Vector[Ticket],
  ):
    private def validNearbyTickets: Vector[Ticket] =
      nearbyTickets.filter(_.isValid(fields))

    def fieldIndices: Range = yourTicket.numbers.indices

    def fieldValues(index: Index): Vector[Value] =
      yourTicket.fieldValue(index) +: validNearbyTickets.map(
        _.fieldValue(index)
      )

  def parse(input: Vector[String]): Data =
    def parseField(input: String): Field =
      val FieldRE: Regex = """([\w\s]+): (\d+)-(\d+) or (\d+)-(\d+)""".r
      input match
        case FieldRE(name, a, b, c, d) =>
          Field(
            name,
            Set(Interval(a.toInt, b.toInt), Interval(c.toInt, d.toInt)),
          )

    def parseTicket(ticket: String): Ticket =
      Ticket(ticket.split(",").map(_.toInt).toVector)

    val firstEmpty    = input.indexWhere(_.isEmpty)
    val secondEmpty   = input.indexWhere(_.isEmpty, firstEmpty + 1)
    val fields        = input.take(firstEmpty).toSet
    val yourTicket    = input(firstEmpty + 2)
    val nearbyTickets = input.drop(secondEmpty + 2)

    Data(
      fields = fields map parseField,
      yourTicket = parseTicket(yourTicket),
      nearbyTickets = nearbyTickets map parseTicket,
    )

  def solve1(data: Data): Int =
    val invalidNumbers = for
      ticket <- data.nearbyTickets
      number <- ticket.invalidNumbers(data.fields)
    yield number

    invalidNumbers.sum

  def solve2(data: Data, prefix: String): Long =
    val candidateIndices: List[(Name, List[Index])] =
      data.fields.toList
        .map { field =>
          val suitableIndices = data.fieldIndices.filter { index =>
            data.fieldValues(index) forall field.isValid
          }.toList

          field.name -> suitableIndices
        }
        .sortBy { case (_, indices) => indices.length }

    @tailrec
    def assignIndices(
      candidateIndices: List[
        (Name, List[Index])
      ], // sorted by length of indices
      acc: List[(Name, Index)] = Nil,
    ): Seq[(Name, Index)] =
      candidateIndices match
        case Nil     => acc
        case x :: xs =>
          val (name, numbers) = x
          numbers match
            case identified :: Nil =>
              assignIndices(
                // removing the `identified` number from elsewhere
                xs.map { case (name, numbers) =>
                  name -> numbers.filterNot(_ == identified)
                },
                name -> identified :: acc,
              )

            case _ =>
              sys.error(s"Expected exactly 1 number but got $numbers")

    val fieldIndices: Map[Name, Index] = assignIndices(candidateIndices).toMap

    assert(
      fieldIndices.values.toSet.size == fieldIndices.size,
      "Indices should not repeat",
    )

    data.fields
      .map(_.name)
      .filter(_.startsWith(prefix))
      .map { name =>
        val index = fieldIndices(name)
        data.yourTicket.fieldValue(index)
      }
      .map(_.toLong)
      .product

  override def parseInput(lines: Iterator[Name]): Either[ErrorMessage, Data] =
    parse(lines.toVector).asRight

  override def solution1(input: Data): Value = solve1(input)

  override def solution2(input: Data): Long = solve2(input, "departure")
