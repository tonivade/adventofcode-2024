package day7

import Day7._

class Day7Suite extends munit.FunSuite:

  val input = """190: 10 19
                |3267: 81 40 27
                |83: 17 5
                |156: 15 6
                |7290: 6 8 6 15
                |161011: 16 10 13
                |192: 17 8 14
                |21037: 9 7 18 13
                |292: 11 6 16 20""".stripMargin

  test("Day7 part1") {
    assertEquals(part1(input), 3749L)
  }

  test("Day7 part2") {
    assertEquals(part2(input), 11387L)
  }

