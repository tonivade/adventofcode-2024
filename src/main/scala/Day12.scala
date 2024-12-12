package day12

import scala.io.Source
import scala.collection.mutable
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/12
object Day12:

  enum Direction:
    case UP, DOWN, LEFT, RIGHT
    
  import Direction._
  case class Position(x: Int, y: Int) extends Ordered[Position]:
    def up = Position(x, y - 1)
    def down = Position(x, y + 1)
    def right = Position(x + 1, y)
    def left = Position(x - 1, y)
    def rightUp = Position(x + 1, y - 1)
    def rightDown = Position(x + 1, y + 1)
    def leftUp = Position(x - 1, y - 1)
    def leftDown = Position(x - 1, y + 1)
    def adjacent = Set(up, down, left, right)
    def all = Set(up, down, left, right, rightUp, rightDown, leftUp, leftDown)
    def compare(other: Position): Int = 
      (this, other) match
        case (Position(x1, y1), Position(x2, y2)) =>
          val diffx = x1 - x2
          if (diffx != 0)
            diffx
          else
            y1 - y2

  case class Walker(direction: Direction, position: Position, turns: Int):
    def turn(next: Direction): Walker = Walker(next, position, turns + 1)
    def move: Walker = 
      direction match
        case UP => Walker(direction, position.up, turns)
        case RIGHT => Walker(direction, position.right, turns)
        case DOWN => Walker(direction, position.down, turns)
        case LEFT => Walker(direction, position.left, turns)

  object Walker:
    def apply(path: Set[Position]): Walker = Walker(RIGHT, path.min, 1)
  
  @tailrec
  def step(walker: Walker, path: Set[Position]): Walker =
    if (path.isEmpty)
      walker
    else
      walker match
        case Walker(UP, p, _) if path.contains(p.up) => 
          val next = walker.move
          step(next, path - next.position)
        case Walker(UP, p, _) if path.contains(p.left) => step(walker.turn(LEFT), path)
        case Walker(UP, p, _) if path.contains(p.right) => step(walker.turn(RIGHT), path)

        case Walker(RIGHT, p, _) if path.contains(p.right) => 
          val next = walker.move
          step(next, path - next.position)
        case Walker(RIGHT, p, _) if path.contains(p.up) => step(walker.turn(UP), path)
        case Walker(RIGHT, p, _) if path.contains(p.down) => step(walker.turn(DOWN), path)

        case Walker(DOWN, p, _) if path.contains(p.down) => 
          val next = walker.move
          step(next, path - next.position)
        case Walker(DOWN, p, _) if path.contains(p.left) => step(walker.turn(LEFT), path)
        case Walker(DOWN, p, _) if path.contains(p.right) => step(walker.turn(RIGHT), path)

        case Walker(LEFT, p, _) if path.contains(p.left) => 
          val next = walker.move
          step(next, path - next.position)
        case Walker(LEFT, p, _) if path.contains(p.up) => step(walker.turn(UP), path)
        case Walker(LEFT, p, _) if path.contains(p.down) => step(walker.turn(DOWN), path)

        case _ => throw RuntimeException("unreachable nodes:" + path)

  case class Shape(positions: Set[Position]):
    def area: Int = positions.size
    def perimeter: Int = positions.foldLeft(0):
      case (result, position) => result + position.adjacent.filter(!positions.contains(_)).size
    def sides: Int = 
      if (positions.size == 1)
        4
      else
        val path = positions.flatMap(_.all.filter(!positions.contains(_)))
        val walker = Walker(path)
        step(walker, path - walker.position).turns
    def fence1: Int = area * perimeter
    def fence2: Int = area * sides
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

  def groups(input: String): Iterable[Shape] =
    val matrix = parse(input)
    val colors = matrix.groupMap(_._2)(_._1)
    val result = colors.map:
      case (color, positions) => (color -> search(positions.toSet))
    result.flatMap(_._2)

  def part1(input: String): Int = 
    groups(input).map(_.fence1).sum

  def part2(input: String): Int =
    groups(input).map(_.fence2).sum

@main def main: Unit =
  val input = Source.fromFile("input/day12.txt").getLines().mkString("\n")
  println(Day12.part1(input))
  println(Day12.part2(input))

