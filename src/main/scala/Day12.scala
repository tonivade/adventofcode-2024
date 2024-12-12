package day12

import scala.io.Source

// https://adventofcode.com/2024/day/12
object Day12:
  case class Position(x: Int, y: Int):
    def up = Position(x, y + 1)
    def down = Position(x, y - 1)
    def right = Position(x + 1, y)
    def left = Position(x - 1, y)
    def adjacent = List(up, down, left, right)
    def areClose(other: Position) = adjacent.contains(other)

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

  def neighbors(matrix: Map[Position, Char])(position: Position): Set[Position] = 
    position.adjacent.filter(matrix.contains).filter(matrix(_) == matrix(position)).toSet

  def visit(matrix: Map[Position, Char])(position: Position, visited: Set[Position] = Set.empty): Set[Position] =
    if (visited.contains(position))
      Set.empty
    else
      neighbors(matrix)(position).flatMap(visit(matrix)(_, visited + position)) + position

  def search(matrix: Map[Position, Char])(positions: Iterable[Position]): Set[Shape] =
    positions.foldLeft(Set.empty[Shape]):
      case (shapes, position) if (shapes.exists(_.contains(position))) => println(s"not new $position"); shapes
      case (shapes, position) => println(s"new $position"); shapes + Shape(visit(matrix)(position))

  def part1(input: String): Int = 
    val matrix = parse(input)

    val colors = matrix.groupMap(_._2)(_._1)

    val result = colors.map:
      case (color, positions) => 
        println(s"$color ${positions.size}"); (color -> search(matrix)(positions))
    
    result.flatMap(_._2).map(_.fence).sum

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day12.txt").getLines().mkString("\n")
  println(Day12.part1(input))
  println(Day12.part2(input))

