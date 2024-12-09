package jurisk.adventofcode.y2020

import cats.implicits.*
import jurisk.adventofcode.y2020.Advent04.Ecl

import scala.io.Source
import scala.util.Try

object Advent04 extends App:
  extension (self: String)
    private def between(start: Int, end: Int): Option[Int] =
      self.toIntOption flatMap { x =>
        if ((x >= start) && (x <= end)) x.some else none
      }

  private type Key = String
  case class Extractor[T](
    key: Key,
    extract: String => Option[T],
  )

  private object Extractor:
    def apply[T](key: Key, extractPartial: PartialFunction[String, Option[T]]): Extractor[T] =
      new Extractor(
        key,
        x => if extractPartial.isDefinedAt(x) then extractPartial(x) else none,
      )

    def apply[T](key: Key, extract: String => Option[T]): Extractor[T] =
      new Extractor(key, extract)

  private type Passport = Map[Key, String]

  private val Year = """(\d{4})""".r
  private def year(start: Int, end: Int): PartialFunction[String, Option[Int]] =
    case Year(year) => year.between(start, end)
  
  //  byr (Birth Year) - four digits; at least 1920 and at most 2002.
  private val byr = Extractor("byr", year(1920, 2002))

  //  iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  val Iyr = """(\d{4})""".r
  private val iyr = Extractor("iyr", year(2010, 2020))

  //  eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  val Eyr = """(\d{4})""".r
  private val eyr = Extractor("eyr", year(2020, 2030))

  //  hgt (Height) - a number followed by either cm or in:
  //    If cm, the number must be at least 150 and at most 193.
  //    If in, the number must be at least 59 and at most 76.
  private val HgtCm = """(\d+)cm""".r
  private val HgtIn = """(\d+)in""".r
  private val hgt = Extractor("hgt", {
    case HgtCm(x) => x.between(150, 193)
    case HgtIn(x) => x.between(59, 76)
  })

  //  hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  private val Hcl = """(#[0-9a-f]{6})""".r
  private val hcl = Extractor("hcl", { case Hcl(x) => x.some })

  //  ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  enum Ecl:
    case amb, blu, brn, gry, grn, hzl, oth
  
  private val ecl = Extractor("ecl", x => Try(Ecl.valueOf(x)).toOption)

  //  pid (Passport ID) - a nine-digit number, including leading zeroes.
  private val Pid = """(\d{9})""".r
  private val pid = Extractor("pid", { case Pid(x) => x.toIntOption })

  private val Extractors = Set(byr, iyr, eyr, hgt, hcl, ecl, pid)

  private def valid1(passport: Passport): Boolean =
    (Extractors.map(_.key) -- passport.keySet).isEmpty

  private def valid2(passport: Passport): Boolean =
    Extractors.forall { extractor =>
      passport.get(extractor.key).exists { x =>
        extractor.extract(x).isDefined 
      }
    }

  private val Pair = """(\w+):(.+)""".r
  private val passports =
    Source.fromResource("2020/04.txt")
      .getLines()
      .mkString("\n")
      .split("\n\n")
      .map(_
        .split("""\s""")
        .map {
          case Pair(a, b) => a -> b
          case x          => sys.error(s"Unexpected $x")
        }
        .toMap
      )

  def solve(f: Passport => Boolean): Unit =
    println(passports.count(f))
  
  List(valid1, valid2) foreach solve
