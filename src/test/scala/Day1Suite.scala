package day1

import Day1._

class Day1Suite extends munit.FunSuite:

  val input = """3   4
                |4   3
                |2   5
                |1   3
                |3   9
                |3   3""".stripMargin

  test("Day1 part1") {
    assertEquals(part1(input), 11)
  }

  test("Day1 part2") {
    assertEquals(part2(input), 1)
  }

