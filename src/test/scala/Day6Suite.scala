package day6

import Day6._

class Day6Suite extends munit.FunSuite:

  val input = """....#.....
                |.........#
                |..........
                |..#.......
                |.......#..
                |..........
                |.#..^.....
                |........#.
                |#.........
                |......#...""".stripMargin

  test("Day6 part1") {
    assertEquals(part1(input), 41)
  }

  test("Day6 part2") {
    assertEquals(part2(input), 1)
  }

