package jurisk.adventofcode.y2016

import cats.effect.{IO, IOApp}
import cats.implicits._
import fs2.{Chunk, Stream}
import jurisk.cryptography.Hashing

import scala.annotation.tailrec

object Advent14 extends IOApp.Simple {
  private val TestSalt = "abc"
  private val RealSalt = "ahsbgdzn"

  private def charNTimes(char: Char, times: Int): String =
    List.fill(times)(char).mkString

  @tailrec
  private def hasRepeatedChar(x: String, times: Int): Option[Char] =
    x.headOption match {
      case Some(ch) =>
        val seek: String = charNTimes(ch, times)
        if (x.startsWith(seek)) {
          ch.some
        } else {
          hasRepeatedChar(x.dropWhile(_ == ch), times)
        }
      case None     =>
        None
    }

  private def hasRepeatedChar(x: String, char: Char, times: Int): Boolean =
    x.contains(charNTimes(char, times))

  private def validWindow(list: Chunk[String]): Boolean = list.head exists {
    ch =>
      hasRepeatedChar(ch, 3) exists { char =>
        list.drop(1).exists(s => hasRepeatedChar(s, char, 5))
      }
  }

  private def hash(salt: String, x: Int, additionalHashings: Int): String =
    (0 until additionalHashings).foldLeft(Hashing.md5(salt + x.toString)) {
      case (acc, _) =>
        Hashing.md5(acc)
    }

  private def solve(salt: String, additionalHashings: Int): IO[Option[Int]] = {
    val md5Hashes =
      Stream.iterate(0)(_ + 1).map(x => (x, hash(salt, x, additionalHashings)))

    val ConsiderNextN = 1000

    md5Hashes
      .sliding(ConsiderNextN + 1)
      .map { chunk =>
        val (idx, _) = chunk.head.get
        val hashes   = chunk.map { case (_, hash) => hash }
        (idx, hashes)
      }
      .filter { case (_, hashes) =>
        validWindow(hashes)
      }
      .map { case (idx, _) =>
        idx
      }
      .take(64)
      .last
      .covary[IO]
      .compile
      .onlyOrError
  }

  private def part1(salt: String): IO[Option[Int]] = solve(salt, 0)
  private def part2(salt: String): IO[Option[Int]] = solve(salt, 2016)

  override def run: IO[Unit] = for {
    testResult1 <- part1(TestSalt)
    _            = assert(testResult1 == 22728.some)
    realResult1 <- part1(RealSalt)
    _           <- IO.println(s"Part 1: $realResult1")
    _            = assert(realResult1 == 23890.some)

    testResult2 <- part2(TestSalt)
    _            = assert(testResult2 == 22859.some)
    realResult2 <- part2(RealSalt)
    _           <- IO.println(s"Part 2: $realResult2")
    _            = assert(realResult2 == 22696.some)
  } yield ()
}
