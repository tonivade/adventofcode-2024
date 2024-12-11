package day11

import Day11._

class Day11Suite extends munit.FunSuite:

  test("blink 6 times"):
    assertEquals(blink(List(125L, 17L), 6), 
      List(
        2097446912L, 14168L, 4048L, 2L, 0L, 2L, 4L, 40L, 48L, 2024L, 
        40L, 48L, 80L, 96L, 2L, 8L, 6L, 7L, 6L, 0L, 3L, 2L))

  test("Day11 part1"):
    assertEquals(part1("125 17"), 55312)

  test("Day11 part2".ignore):
    assertEquals(part2("125 17"), 1)

