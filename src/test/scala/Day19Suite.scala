package day19

import Day19._

class Day19Suite extends munit.FunSuite:

  val input = """r, wr, b, g, bwu, rb, gb, br
                |
                |brwrr
                |bggr
                |gbbr
                |rrbgbr
                |ubwu
                |bwurrg
                |brgr
                |bbrgwb""".stripMargin

  test("Day19 part1"):
    assertEquals(part1(input), 6)

  test("Day19 part2"):
    assertEquals(part2(input), 16L)

