package day3

import Day3._

class Day3Suite extends munit.FunSuite:

  val input = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))""".stripMargin

  test("Day3 part1") {
    assertEquals(part1(input), 161)
  }

  test("Day3 part2") {
    assertEquals(part2(input), 1)
  }

