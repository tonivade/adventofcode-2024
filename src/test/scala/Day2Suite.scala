package day2

import Day2._

class Day2Suite extends munit.FunSuite:

  val input = """7 6 4 2 1
                |1 2 7 8 9
                |9 7 6 2 1
                |1 3 2 4 5
                |8 6 4 4 1
                |1 3 6 7 9""".stripMargin

  test("Day2 part1") {
    assertEquals(part1(input), 2)
  }

  test("Day2 part2") {
    assertEquals(part2(input), 1)
  }

