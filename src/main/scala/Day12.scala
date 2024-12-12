package day12

import scala.io.Source
import scala.collection.mutable

// https://adventofcode.com/2024/day/12
object Day12:
  case class Position(x: Int, y: Int):
    def up = Position(x, y + 1)
    def down = Position(x, y - 1)
    def right = Position(x + 1, y)
    def left = Position(x - 1, y)
    def adjacent = Set(up, down, left, right)

  case class Shape(positions: Set[Position]):
    def area: Int = positions.size
    def perimeter: Int = positions.foldLeft(0):
      case (result, position) => result + position.adjacent.filter(!positions.contains(_)).size
    def fence: Int = area * perimeter
    def contains(p: Position): Boolean = positions.contains(p)

  def parse(input: String): Map[Position, Char] = 
    input.linesIterator.zipWithIndex.flatMap:
      case (line, y) => line.zipWithIndex.map:
        (ch, x) => Position(x, y) -> ch
    .toMap

  def search(positions: Set[Position]): Set[Shape] =
    val visited = mutable.Set.empty[Position]

    def neighbors(positions: Set[Position])(current: Position): Set[Position] = 
      current.adjacent.filter(positions.contains)

    def visit(positions: Set[Position])(current: Position): Set[Position] =
      if (visited.contains(current))
        Set.empty
      else
        visited.addOne(current)
        neighbors(positions)(current).flatMap(visit(positions)) + current
    
    positions.foldLeft(Set.empty[Shape]):
      case (shapes, position) if (shapes.exists(_.contains(position))) => shapes
      case (shapes, position) => shapes + Shape(visit(positions)(position))

  def part1(input: String): Int = 
    val matrix = parse(input)

    val colors = matrix.groupMap(_._2)(_._1)

    val result = colors.map:
      case (color, positions) => (color -> search(positions.toSet))
    
    result.flatMap(_._2).map(_.fence).sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day12.txt").getLines().mkString("\n")
  println(Day12.part1(input))
  println(Day12.part2(input))

