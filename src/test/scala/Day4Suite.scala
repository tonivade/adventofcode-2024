package day4

import Day4._

class Day4Suite extends munit.FunSuite:

  val input = """MMMSXXMASM
                |MSAMXMSMSA
                |AMXSXMAAMM
                |MSAMASMSMX
                |XMASAMXAMM
                |XXAMMXXAMA
                |SMSMSASXSS
                |SAXAMASAAA
                |MAMMMXMMMM
                |MXMXAXMASX""".stripMargin

  test("Day4 part1") {
    assertEquals(part1(input), 18)
  }

  test("Day4 part2") {
    assertEquals(part2(input), 1)
  }

