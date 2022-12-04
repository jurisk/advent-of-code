package jurisk.adventofcode.y2021

import cats.implicits._
import scala.util.Try

object Advent16 {
  type VersionNumber = Byte
  type Error = String
  type Bits = List[Byte]

  private def binaryToInt(s: String): Int = {
    Integer.parseInt(s, 2)
  }

  private def binaryToLong(s: String): Long = {
    java.lang.Long.parseLong(s, 2)
  }

  private def fourBits(a: Byte, b: Byte, c: Byte, d: Byte): String = {
    s"$a$b$c$d"
  }

  private def threeBits(a: Byte, b: Byte, c: Byte): Byte = {
    (a * 4 + b * 2 + c).toByte
  }

  def toBinaryList(i: Int, digits: Int): Bits =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0').toList.map(x => (x - '0').toByte)

  def binaryListToInt(list: Bits): Int = {
    binaryToInt(list.map(x => s"$x").mkString)
  }

  sealed trait Packet {
    def allVersionNumbers: Int
    def evaluate: Long
  }

  object Packet {
    case class Literal(versionNumber: VersionNumber, number: Long) extends Packet {
      def allVersionNumbers: Int =
        versionNumber

      def evaluate: Long = number
    }

    object Literal {
      private def parseNumber(binary: Bits): Either[Error, (Long, Bits)] = {
        def helper(binary: Bits): Either[Error, (String, Bits)] = {
          binary match {
            case 0 :: a :: b :: c :: d :: tail => Right(fourBits(a, b, c, d), tail)
            case 1 :: a :: b :: c :: d :: tail => helper(tail).map { case (n, t) => (fourBits(a, b, c, d) + n, t) }
            case _ => Left(s"Unexpected $binary")
          }
        }

        helper(binary).map { case (n, rem) => (binaryToLong(n), rem) }
      }

      def parse(version: VersionNumber, binary: Bits): Either[Error, (Packet, Bits)] = {
        parseNumber(binary).map { case (n, rem) => (Packet.Literal(version, n), rem) }
      }
    }

    sealed trait Operator extends Packet {
      def versionNumber: VersionNumber
      def subPackets: List[Packet]
      def allVersionNumbers: Int =
        versionNumber + subPackets.map(_.allVersionNumbers).sum
    }

    case class Sum(versionNumber: VersionNumber, subPackets: List[Packet]) extends Operator {
      def evaluate: Long = subPackets.map(_.evaluate).sum
    }

    case class Product(versionNumber: VersionNumber, subPackets: List[Packet]) extends Operator {
      def evaluate: Long = subPackets.map(_.evaluate).product
    }

    case class Minimum(versionNumber: VersionNumber, subPackets: List[Packet]) extends Operator {
      def evaluate: Long = subPackets.map(_.evaluate).min
    }

    case class Maximum(versionNumber: VersionNumber, subPackets: List[Packet]) extends Operator {
      def evaluate: Long = subPackets.map(_.evaluate).max
    }

    sealed trait BinaryOperator extends Operator {
      def a: Packet
      def b: Packet

      def subPackets: List[Packet] = a :: b :: Nil
    }

    case class GreaterThan(versionNumber: VersionNumber, a: Packet, b: Packet) extends BinaryOperator {
      def evaluate: Long = if (a.evaluate > b.evaluate) 1 else 0
    }

    case class LessThan(versionNumber: VersionNumber, a: Packet, b: Packet) extends BinaryOperator {
      def evaluate: Long = if (a.evaluate < b.evaluate) 1 else 0
    }

    case class EqualTo(versionNumber: VersionNumber, a: Packet, b: Packet) extends BinaryOperator {
      def evaluate: Long = if (a.evaluate == b.evaluate) 1 else 0
    }

    object Operator {
      def create(version: VersionNumber, typeId: Byte, subPackets: List[Packet]): Either[Error, Packet] = typeId match {
        case 0 => Right(Sum(version, subPackets))
        case 1 => Right(Product(version, subPackets))
        case 2 => Right(Minimum(version, subPackets))
        case 3 => Right(Maximum(version, subPackets))
        case 5 if subPackets.length == 2 => Right(GreaterThan(version, subPackets.head, subPackets(1)))
        case 6 if subPackets.length == 2 => Right(LessThan(version, subPackets.head, subPackets(1)))
        case 7 if subPackets.length == 2 => Right(EqualTo(version, subPackets.head, subPackets(1)))
        case _ => Left(s"Unknown type $typeId $subPackets")
      }

      def parse(version: VersionNumber, typeId: Byte, binary: Bits): Either[Error, (Packet, Bits)] = {
        val subPackets: Either[Error, (List[Packet], Bits)] = binary match {
          // If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
          case 0 :: tail if tail.length > 15 =>
            val (h, t) = tail.splitAt(15)
            val bitsForPackets = binaryListToInt(h)
            val (bitsToParse, rem) = t.splitAt(bitsForPackets)
            parseBitsIntoPackets(bitsToParse).map { packets => (packets, rem) }

          // If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
          case 1 :: tail if tail.length > 11 =>
            val (h, t) = tail.splitAt(11)
            parsePacketsByNumber(binaryListToInt(h), t)

          case _ =>
            Left(s"Unexpected $binary")
        }

        for {
          (subPackets, rem) <- subPackets
          packet <- Packet.Operator.create(version, typeId, subPackets)
        } yield (packet, rem)
    }
  }

    private def parseHex(ch: Char): Either[Error, Int] = {
      Try(Integer.parseInt(s"$ch", 16)).toEither.leftMap(e => s"Failed to parse $ch: $e")
    }

    private def hexCharToBinaryList(ch: Char): Either[Error, Bits] = for {
      n <- parseHex(ch)
    } yield toBinaryList(n, 4)

    def parseBitsIntoPackets(binary: Bits): Either[Error, List[Packet]] = {
      if (binary.isEmpty) {
        Right(Nil)
      } else {
        for {
          (packet, rem) <- Packet.parsePacket(binary)
          packets <- parseBitsIntoPackets(rem)
        } yield packet :: packets
      }
    }

    def parsePacketsByNumber(howMany: Int, binary: Bits): Either[Error, (List[Packet], Bits)] = {
      if (howMany == 0) {
        Right((Nil, binary))
      } else {
        for {
          (packet, rem1) <- Packet.parsePacket(binary)
          (morePackets, rem2) <- Packet.parsePacketsByNumber(howMany - 1, rem1)
        } yield (packet :: morePackets, rem2)
      }
    }


    def parsePacket(binary: Bits): Either[Error, (Packet, Bits)] = {
      binary match {
        case a :: b :: c :: 1 :: 0 :: 0 :: tail => Packet.Literal.parse(threeBits(a, b, c), tail)
        case a :: b :: c :: d :: e :: f :: tail => Packet.Operator.parse(threeBits(a, b, c), threeBits(d, e, f), tail)
        case _ => Left(s"Unexpected $binary")
      }
    }

    def parseSingle(input: String): Either[Error, Packet] = for {
      binary <- input.toList.traverse(hexCharToBinaryList)
      parsed <- parsePacket(binary.flatten)
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

  val RealInput = "220D4B80491FE6FBDCDA61F23F1D9B763004A7C128012F9DA88CE27B000B30F4804D49CD515380352100763DC5E8EC000844338B10B667A1E60094B7BE8D600ACE774DF39DD364979F67A9AC0D1802B2A41401354F6BF1DC0627B15EC5CCC01694F5BABFC00964E93C95CF080263F0046741A740A76B704300824926693274BE7CC880267D00464852484A5F74520005D65A1EAD2334A700BA4EA41256E4BBBD8DC0999FC3A97286C20164B4FF14A93FD2947494E683E752E49B2737DF7C4080181973496509A5B9A8D37B7C300434016920D9EAEF16AEC0A4AB7DF5B1C01C933B9AAF19E1818027A00A80021F1FA0E43400043E174638572B984B066401D3E802735A4A9ECE371789685AB3E0E800725333EFFBB4B8D131A9F39ED413A1720058F339EE32052D48EC4E5EC3A6006CC2B4BE6FF3F40017A0E4D522226009CA676A7600980021F1921446700042A23C368B713CC015E007324A38DF30BB30533D001200F3E7AC33A00A4F73149558E7B98A4AACC402660803D1EA1045C1006E2CC668EC200F4568A5104802B7D004A53819327531FE607E118803B260F371D02CAEA3486050004EE3006A1E463858600F46D8531E08010987B1BE251002013445345C600B4F67617400D14F61867B39AA38018F8C05E430163C6004980126005B801CC0417080106005000CB4002D7A801AA0062007BC0019608018A004A002B880057CEF5604016827238DFDCC8048B9AF135802400087C32893120401C8D90463E280513D62991EE5CA543A6B75892CB639D503004F00353100662FC498AA00084C6485B1D25044C0139975D004A5EB5E52AC7233294006867F9EE6BA2115E47D7867458401424E354B36CDAFCAB34CBC2008BF2F2BA5CC646E57D4C62E41279E7F37961ACC015B005A5EFF884CBDFF10F9BFF438C014A007D67AE0529DED3901D9CD50B5C0108B13BAFD6070"

  def main(args: Array[String]): Unit = {
    val result1 = solve1(RealInput)
    println(result1)

    val result2 = solve2(RealInput)
    println(result2)
  }
}
