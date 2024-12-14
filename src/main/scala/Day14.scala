package day14

import scala.io.Source
import scala.annotation.tailrec
import scala.io.StdIn.readLine

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

  def mkString(robots: List[Robot], bounds: Bounds): String = 
    val occupied = robots.groupBy(_.position).keySet
    val buffer = new StringBuffer
    buffer.append(" ")
    for (x <- 0 until bounds.width)
      buffer.append(x % 10)
    buffer.append("\n")
    for (y <- 0 until bounds.height)
      buffer.append(y % 10)
      for (x <- 0 until bounds.width)
        buffer.append(if occupied.contains(Position(x, y)) then "#" else ".")
      buffer.append("\n")
    buffer.toString

  @tailrec
  def walk(robots: List[Robot], bounds: Bounds, steps: Int): List[Robot] = 
    if (steps > 0)
      walk(robots.map(_.step(bounds)), bounds, steps - 1)
    else
      robots

  def visibleRobots(robots: List[Robot]): Int =
    robots.groupBy(_.position).size
  
  def walk2(robots: List[Robot], bounds: Bounds, steps: Int, visible: Map[Int, Int] = Map.empty): Map[Int, Int] = 
    if (steps > 0)
      walk2(robots.map(_.step(bounds)), bounds, steps - 1, visible + (steps -> visibleRobots(robots)))
    else
      visible

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

  def part2(input: String): Int = 
    val robots = parse(input)
    val bounds = Bounds(103, 101)
    val result = 10000 - walk2(robots, bounds, 10000).maxBy(_._2)._1
    println(mkString(walk(robots, bounds, result), bounds))
    result

@main def main: Unit =
  val input = Source.fromFile("input/day14.txt").getLines().mkString("\n")
  println(Day14.part1(input))
  println(Day14.part2(input))

