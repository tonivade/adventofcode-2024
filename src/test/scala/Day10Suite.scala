package day10

import Day10._

class Day10Suite extends munit.FunSuite:

  val input = """89010123
                |78121874
                |87430965
                |96549874
                |45678903
                |32019012
                |01329801
                |10456732""".stripMargin

  test("Day10 part1"):
    assertEquals(part1(input), 36)

  test("Day10 part2"):
    assertEquals(part2(input), 81)

