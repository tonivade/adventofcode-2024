package day17

import Day17._

class Day17Suite extends munit.FunSuite:

  val input = """Register A: 729
                |Register B: 0
                |Register C: 0
                |
                |Program: 0,1,5,4,3,0""".stripMargin

  val input2 = """Register A: 2024
                 |Register B: 0
                 |Register C: 0
                 |
                 |Program: 0,3,5,4,3,0""".stripMargin

  test("Day17 part1 program examples"):
    assertEquals(exec(Vector(0, 2))(Computer(10, 0, 0)).a, 2)
    assertEquals(exec(Vector(6, 2))(Computer(10, 0, 0)).b, 2)
    assertEquals(exec(Vector(7, 2))(Computer(10, 0, 0)).c, 2)
    assertEquals(exec(Vector(2, 6))(Computer(0, 0, 9)).b, 1)
    assertEquals(exec(Vector(5, 0, 5, 1, 5, 4))(Computer(10, 0, 0)).output, List(0, 1, 2))
    assertEquals(exec(Vector(1, 7))(Computer(0, 29, 0)).b, 26)
    assertEquals(exec(Vector(4, 0))(Computer(0, 2024, 43690)).b, 44354)
    val result = exec(Vector(0, 1, 5, 4, 3, 0))(Computer(2024, 0, 0))
    assertEquals(result.a, 0)
    assertEquals(result.output, List(4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0))

  test("Day17 part1"):
    assertEquals(part1(input), "4,6,3,5,6,3,5,2,1,0")

  test("Day17 part2 toNumber"):
    assertEquals(toNumber(List(5, 7, 3, 0)), 253L)
    assertEquals(toNumber(List(0, 3, 5, 4, 3, 0)), 14680L)

  test("Day17 part2"):
    assertEquals(part2(input2), 117440L)

