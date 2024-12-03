package day3

import Day3._

class Day3Suite extends munit.FunSuite:

  val input1 = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
  val input2 = """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

  test("Day3 part1") {
    assertEquals(part1(input1), 161)
  }

  test("Day3 part2") {
    assertEquals(part2(input2), 48)
  }

