package day18

import Day18._

class Day18Suite extends munit.FunSuite:

  val input = """5,4
                |4,2
                |4,5
                |3,0
                |2,1
                |6,3
                |2,4
                |1,5
                |0,6
                |3,3
                |2,6
                |5,1
                |1,2
                |5,5
                |2,5
                |6,5
                |1,4
                |0,4
                |6,4
                |1,1
                |6,1
                |1,0
                |0,5
                |1,6
                |2,0""".stripMargin

  test("Day18 part1"):
    assertEquals(solve1(input, Bounds(7, 7), 12), 22)

  test("Day18 part2".ignore):
    assertEquals(part2(input), 1)

