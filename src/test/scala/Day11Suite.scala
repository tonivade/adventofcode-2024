package day11

import Day11._

class Day11Suite extends munit.FunSuite:

  test("Day11 part1"):
    assertEquals(part1("125 17"), 55312L  )

  test("Day11 part2"):
    assertEquals(part2("125 17"), 65601038650482L)

