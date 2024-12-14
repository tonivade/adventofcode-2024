package day14

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/14
object Day14:

  case class Bounds(height: Int, width: Int):
    val midX = width / 2
    val midY = height / 2

    def teleport(position: Position): Position =
      val nextX = 
        if (position.x < 0) 
          width + position.x
        else if (position.x >= width)
          position.x - width
        else position.x

      val nextY = 
        if (position.y < 0)
          height + position.y
        else if (position.y >= height)
          position.y - height
        else position.y

      Position(nextX, nextY)

    def quadrant(robot: Robot): Int =

      robot.position match
        case (Position(x, y)) if (x < midX && y < midY) => 0
        case (Position(x, y)) if (x > midX && y < midY) => 1
        case (Position(x, y)) if (x < midX && y > midY) => 2
        case (Position(x, y)) if (x > midX && y > midY) => 3
        case _ => -1
      
  case class Position(x: Int, y: Int)
  case class Robot(position: Position, speed: Position):
    def step(bounds: Bounds): Robot = 
      val nextX = position.x + speed.x
      val nextY = position.y + speed.y
      Robot(bounds.teleport(Position(nextX, nextY)), speed)

  @tailrec
  def walk(robots: List[Robot], bounds: Bounds, steps: Int): List[Robot] = 
    if (steps > 0)
      walk(robots.map(_.step(bounds)), bounds, steps - 1)
    else
      robots

  def parseRobot(line: String): Robot = 
    val regex = """p=(\-?\d+),(\-?\d+) v=(\-?\d+),(\-?\d+)""".r
    line match
      case regex(px, py, vx, vy) => 
        Robot(Position(px.toInt, py.toInt), Position(vx.toInt, vy.toInt))

  def quadrants(robots: List[Robot], bounds: Bounds, seconds: Int): Map[Int, Int] = 
    walk(robots, bounds, seconds)
      .groupMapReduce(bounds.quadrant)(_ => 1)(_ + _).filterKeys(_ >= 0)
      .toMap

  def parse(input: String): List[Robot] =
    input.linesIterator.map(parseRobot).toList

  def part1(input: String): Int = 
    quadrants(parse(input), Bounds(103, 101), 100).values.foldLeft(1)(_ * _)

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day14.txt").getLines().mkString("\n")
  println(Day14.part1(input))
  println(Day14.part2(input))

