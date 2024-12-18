package day17

import Day17._

class Day17Suite extends munit.FunSuite:

  val input = """Register A: 729
                |Register B: 0
                |Register C: 0
                |
                |Program: 0,1,5,4,3,0""".stripMargin

  test("Day17 part1 program examples"):
    assertEquals(exec(Computer(10, 0, 0))(Vector(0, 2)).a, 2)
    assertEquals(exec(Computer(10, 0, 0))(Vector(6, 2)).b, 2)
    assertEquals(exec(Computer(10, 0, 0))(Vector(7, 2)).c, 2)
    assertEquals(exec(Computer(0, 0, 9))(Vector(2, 6)).b, 1)
    assertEquals(exec(Computer(10, 0, 0))(Vector(5, 0, 5, 1, 5, 4)).output, List(0, 1, 2))
    assertEquals(exec(Computer(0, 29, 0))(Vector(1, 7)).b, 26)
    assertEquals(exec(Computer(0, 2024, 43690))(Vector(4, 0)).b, 44354)
    val result = exec(Computer(2024, 0, 0))(Vector(0, 1, 5, 4, 3, 0))
    assertEquals(result.a, 0)
    assertEquals(result.output, List(4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0))

  test("Day17 part1"):
    assertEquals(part1(input), "4,6,3,5,6,3,5,2,1,0")

  test("Day17 part2".ignore):
    assertEquals(part2(input), 1)

