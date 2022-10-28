import org.scalatest._
import flatspec._
import Solution16._
import Solution16.Packet._
import org.scalatest.matchers.should.Matchers._

class Solution16Spec extends AnyFlatSpec {
  "Solution 16" should "parse D2FE28" in {
    parseSingle("D2FE28") shouldEqual Right(Literal(6, 2021))
  }

  it should "parse 38006F45291200" in {
    parseSingle("38006F45291200") shouldEqual Right(LessThan(1, Literal(6, 10), Literal(2, 20)))
  }

  it should "parse EE00D40C823060" in {
    parseSingle("EE00D40C823060") shouldEqual Right(Maximum(7, List(Literal(2, 1), Literal(4, 2), Literal(1, 3))))
  }

  it should "solve part 1 for 8A004A801A8002F478" in {
    solve1("8A004A801A8002F478") shouldEqual Right(16)
  }

  it should "solve part 1 for 620080001611562C8802118E34" in {
    solve1("620080001611562C8802118E34") shouldEqual Right(12)
  }

  it should "solve part 1 for C0015000016115A2E0802F182340" in {
    solve1("C0015000016115A2E0802F182340") shouldEqual Right(23)
  }

  it should "solve part 1 for A0016C880162017C3686B18A3D4780" in {
    solve1("A0016C880162017C3686B18A3D4780") shouldEqual Right(31)
  }

  it should "work for real part 1" in {
    solve1(RealInput) shouldEqual Right(977)
  }

  it should "eval C200B40A82" in {
    solve2("C200B40A82") shouldEqual Right(3)
  }

  it should "eval 04005AC33890" in {
    solve2("04005AC33890") shouldEqual Right(54)
  }

  it should "eval 880086C3E88112" in {
    solve2("880086C3E88112") shouldEqual Right(7)
  }

  it should "eval CE00C43D881120" in {
    solve2("CE00C43D881120") shouldEqual Right(9)
  }

  it should "eval D8005AC2A8F0" in {
    solve2("D8005AC2A8F0") shouldEqual Right(1)
  }

  it should "eval F600BC2D8F" in {
    solve2("F600BC2D8F") shouldEqual Right(0)
  }

  it should "eval 9C005AC2F8F0" in {
    solve2("9C005AC2F8F0") shouldEqual Right(0)
  }

  it should "eval 9C0141080250320F1802104A08" in {
    solve2("9C0141080250320F1802104A08") shouldEqual Right(1)
  }

  it should "work for real part 2" in {
    solve2(RealInput) shouldEqual Right(101501020883L)
  }
}
