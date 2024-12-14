package day14

import Day14._

class Day14Suite extends munit.FunSuite:

  val input = """p=0,4 v=3,-3
                |p=6,3 v=-1,-3
                |p=10,3 v=-1,2
                |p=2,0 v=2,-1
                |p=0,0 v=1,3
                |p=3,0 v=-2,-2
                |p=7,6 v=-1,-3
                |p=3,0 v=-1,-2
                |p=9,3 v=2,3
                |p=7,3 v=-1,2
                |p=2,4 v=2,-3
                |p=9,5 v=-3,-3""".stripMargin
  
  test("move robot example"):
    val robot = Robot(Position(2, 4), Position(2, -3))
    val bounds = Bounds(7, 11)
    assertEquals(robot.step(bounds).position, Position(4, 1))
    assertEquals(robot.step(bounds).step(bounds).position, Position(6, 5))
    assertEquals(robot.step(bounds).step(bounds).step(bounds).position, Position(8, 2))
    assertEquals(robot.step(bounds).step(bounds).step(bounds).step(bounds).position, Position(10, 6))
    assertEquals(robot.step(bounds).step(bounds).step(bounds).step(bounds).step(bounds).position, Position(1, 3))

  test("Day14 part1"):
    val result = quadrants(parse(input), Bounds(7, 11), 100).values.foldLeft(1)(_ * _)
    assertEquals(result, 12)

