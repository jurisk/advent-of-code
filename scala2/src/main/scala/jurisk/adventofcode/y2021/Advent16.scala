package jurisk.adventofcode.y2021

import cats.implicits._
import jurisk.utils.FileInput.readSingleFileLine
import jurisk.utils.Parsing.StringOps

import scala.util.Try

object Advent16 {
  type VersionNumber = Byte
  type Error         = String
  type Bits          = List[Byte]

  private def binaryToInt(s: String): Int =
    Integer.parseInt(s, 2)

  private def binaryToLong(s: String): Long =
    java.lang.Long.parseLong(s, 2)

  private def fourBits(a: Byte, b: Byte, c: Byte, d: Byte): String =
    s"$a$b$c$d"

  private def threeBits(a: Byte, b: Byte, c: Byte): Byte =
    (a * 4 + b * 2 + c).toByte

  private def toBinaryList(i: Int, digits: Int): Bits =
    String
      .format("%" + digits + "s", i.toBinaryString)
      .replace(' ', '0')
      .toList
      .map(x => (x - '0').toByte)

  private def binaryListToInt(list: Bits): Int =
    binaryToInt(list.map(x => s"$x").mkString)

  sealed trait Packet {
    def allVersionNumbers: Int
    def evaluate: Long
  }

  object Packet {
    final case class Literal(versionNumber: VersionNumber, number: Long)
        extends Packet {
      def allVersionNumbers: Int =
        versionNumber

      def evaluate: Long = number
    }

    object Literal {
      private def parseNumber(binary: Bits): Either[Error, (Long, Bits)] = {
        def helper(binary: Bits): Either[Error, (String, Bits)] =
          binary match {
            case 0 :: a :: b :: c :: d :: tail =>
              (fourBits(a, b, c, d), tail).asRight
            case 1 :: a :: b :: c :: d :: tail =>
              helper(tail).map { case (n, t) => (fourBits(a, b, c, d) + n, t) }
            case _                             => Left(s"Unexpected $binary")
          }

        helper(binary).map { case (n, rem) => (binaryToLong(n), rem) }
      }

      def parse(
        version: VersionNumber,
        binary: Bits,
      ): Either[Error, (Packet, Bits)] =
        parseNumber(binary).map { case (n, rem) =>
          (Packet.Literal(version, n), rem)
        }
    }

    sealed trait Operator extends Packet {
      def versionNumber: VersionNumber
      def subPackets: List[Packet]
      def allVersionNumbers: Int =
        versionNumber + subPackets.map(_.allVersionNumbers).sum
    }

    object Operator {
      final case class Sum(
        versionNumber: VersionNumber,
        subPackets: List[Packet],
      ) extends Operator {
        def evaluate: Long = subPackets.map(_.evaluate).sum
      }

      final case class Product(
        versionNumber: VersionNumber,
        subPackets: List[Packet],
      ) extends Operator {
        def evaluate: Long = subPackets.map(_.evaluate).product
      }

      final case class Minimum(
        versionNumber: VersionNumber,
        subPackets: List[Packet],
      ) extends Operator {
        def evaluate: Long = subPackets.map(_.evaluate).min
      }

      final case class Maximum(
        versionNumber: VersionNumber,
        subPackets: List[Packet],
      ) extends Operator {
        def evaluate: Long = subPackets.map(_.evaluate).max
      }

      sealed trait BinaryOperator extends Operator {
        def a: Packet
        def b: Packet

        def subPackets: List[Packet] = a :: b :: Nil
      }

      final case class GreaterThan(
        versionNumber: VersionNumber,
        a: Packet,
        b: Packet,
      ) extends BinaryOperator {
        def evaluate: Long = if (a.evaluate > b.evaluate) 1 else 0
      }

      final case class LessThan(
        versionNumber: VersionNumber,
        a: Packet,
        b: Packet,
      ) extends BinaryOperator {
        def evaluate: Long = if (a.evaluate < b.evaluate) 1 else 0
      }

      final case class EqualTo(
        versionNumber: VersionNumber,
        a: Packet,
        b: Packet,
      ) extends BinaryOperator {
        def evaluate: Long = if (a.evaluate == b.evaluate) 1 else 0
      }

      private def create(
        version: VersionNumber,
        typeId: Byte,
        subPackets: List[Packet],
      ): Either[Error, Packet] = typeId match {
        case 0                           => Right(Sum(version, subPackets))
        case 1                           => Right(Product(version, subPackets))
        case 2                           => Right(Minimum(version, subPackets))
        case 3                           => Right(Maximum(version, subPackets))
        case 5 if subPackets.length == 2 =>
          Right(GreaterThan(version, subPackets.head, subPackets(1)))
        case 6 if subPackets.length == 2 =>
          Right(LessThan(version, subPackets.head, subPackets(1)))
        case 7 if subPackets.length == 2 =>
          Right(EqualTo(version, subPackets.head, subPackets(1)))
        case _                           => Left(s"Unknown type $typeId $subPackets")
      }

      def parse(
        version: VersionNumber,
        typeId: Byte,
        binary: Bits,
      ): Either[Error, (Packet, Bits)] = {
        val subPackets: Either[Error, (List[Packet], Bits)] = binary match {
          // If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
          case 0 :: tail if tail.length > 15 =>
            val (h, t)             = tail.splitAt(15)
            val bitsForPackets     = binaryListToInt(h)
            val (bitsToParse, rem) = t.splitAt(bitsForPackets)
            parseBitsIntoPackets(bitsToParse).map(packets => (packets, rem))

          // If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
          case 1 :: tail if tail.length > 11 =>
            val (h, t) = tail.splitAt(11)
            parsePacketsByNumber(binaryListToInt(h), t)

          case _ =>
            Left(s"Unexpected $binary")
        }

        for {
          (subPackets, rem) <- subPackets
          packet            <- Packet.Operator.create(version, typeId, subPackets)
        } yield (packet, rem)
      }
    }

    private def parseHex(ch: Char): Either[Error, Int] =
      Try(Integer.parseInt(s"$ch", 16)).toEither.leftMap(e =>
        s"$ch $e".failedToParse
      )

    private def hexCharToBinaryList(ch: Char): Either[Error, Bits] = for {
      n <- parseHex(ch)
    } yield toBinaryList(n, 4)

    private def parseBitsIntoPackets(
      binary: Bits
    ): Either[Error, List[Packet]] =
      if (binary.isEmpty) {
        Right(Nil)
      } else {
        for {
          (packet, rem) <- Packet.parsePacket(binary)
          packets       <- parseBitsIntoPackets(rem)
        } yield packet :: packets
      }

    private def parsePacketsByNumber(
      howMany: Int,
      binary: Bits,
    ): Either[Error, (List[Packet], Bits)] =
      if (howMany == 0) {
        Right((Nil, binary))
      } else {
        for {
          (packet, rem1)      <- Packet.parsePacket(binary)
          (morePackets, rem2) <- Packet.parsePacketsByNumber(howMany - 1, rem1)
        } yield (packet :: morePackets, rem2)
      }

    def parsePacket(binary: Bits): Either[Error, (Packet, Bits)] =
      binary match {
        case a :: b :: c :: 1 :: 0 :: 0 :: tail =>
          Packet.Literal.parse(threeBits(a, b, c), tail)
        case a :: b :: c :: d :: e :: f :: tail =>
          Packet.Operator.parse(threeBits(a, b, c), threeBits(d, e, f), tail)
        case _                                  => Left(s"Unexpected $binary")
      }

    def parseSingle(input: String): Either[Error, Packet] = for {
      binary     <- input.toList.traverse(hexCharToBinaryList)
      parsed     <- parsePacket(binary.flatten)
      (result, _) = parsed
    } yield result
  }

  def solve1(input: String): Either[Error, Int] = {
    val parsed = Packet.parseSingle(input)
    parsed.map(_.allVersionNumbers)
  }

  def solve2(input: String): Either[Error, Long] = {
    val parsed = Packet.parseSingle(input)
    parsed.map(_.evaluate)
  }

  val RealInput: String = readSingleFileLine("2021/16.txt")

  def main(args: Array[String]): Unit = {
    val result1 = solve1(RealInput)
    println(result1)

    val result2 = solve2(RealInput)
    println(result2)
  }
}
