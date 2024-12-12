package day12

import Day12._
import java.awt.geom.Area

class Day12Suite extends munit.FunSuite:

  val input = """RRRRIICCFF
                |RRRRIICCCF
                |VVRRRCCFFF
                |VVRCCCJFFF
                |VVVVCJJCFE
                |VVIVCCJJEE
                |VVIIICJJEE
                |MIIIIIJJEE
                |MIIISIJEEE
                |MMMISSJEEE""".stripMargin
  val input1 = """AAAA
                 |BBCD
                 |BBCC
                 |EEEC""".stripMargin
  val input2 = """OOOOO
                 |OXOXO
                 |OOOOO
                 |OXOXO
                 |OOOOO""".stripMargin
  val input3 = """EEEEE
                 |EXXXX
                 |EEEEE
                 |EXXXX
                 |EEEEE""".stripMargin
  val input4 = """AAAAAA
                 |AAABBA
                 |AAABBA
                 |ABBAAA
                 |ABBAAA
                 |AAAAAA""".stripMargin

  test("Day12 shape area"):
    assertEquals(Shape(Set(Position(0, 0), Position(1, 0), Position(2, 0), Position(3, 0))).area, 4)

  test("Day12 shape perimeter"):
    assertEquals(Shape(Set(Position(0, 0), Position(1, 0), Position(2, 0), Position(3, 0))).perimeter, 10)

  test("Day12 shape sides"):
    assertEquals(Shape(Set(Position(0, 0), Position(1, 0), Position(2, 0), Position(3, 0))).sides, 4)

  test("Day12 part1 input1"):
    assertEquals(part1(input1), 140)

  test("Day12 part1 input2"):
    assertEquals(part1(input2), 772)

  test("Day12 part1"):
    assertEquals(part1(input), 1930)

  test("Day12 part2 input1"):
    assertEquals(part2(input1), 80)

  test("Day12 part2 input2"):
    assertEquals(part2(input2), 436)

  test("Day12 part2 input3"):
    assertEquals(part2(input3), 236)

  test("Day12 part2 input4"):
    assertEquals(part2(input4), 368)

  test("Day12 part2".ignore):
    assertEquals(part2(input), 1206)

