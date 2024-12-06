package day6

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/6
object Day6:

  enum Direction:
    case UP, DOWN, LEFT, RIGHT
    
  import Direction._
  
  case class Position(x: Int, y: Int):
    def right = Position(x + 1, y)
    def left = Position(x - 1, y)
    def up = Position(x, y - 1)
    def down = Position(x, y + 1)

  case class Guard(direction: Direction, position: Position):
    def move(next: Direction): Guard = 
      next match
        case UP => Guard(next, position.up)
        case RIGHT => Guard(next, position.right)
        case DOWN => Guard(next, position.down)
        case LEFT => Guard(next, position.left)
    
    def outside(matrix: Map[Position, Char]): Boolean = !matrix.contains(position)

  def parse(input: String): Map[Position, Char] =
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        (ch, x) => Position(x, y) -> ch
    .toMap

  def collision(matrix: Map[Position, Char], next: Position): Boolean =
    matrix.get(next) match
      case Some(ch) => ch == '#'
      case None => false
  
  def step(guard: Guard, matrix: Map[Position, Char]): Guard = 
    guard match
      case Guard(UP, p) if (collision(matrix, p.up)) => guard.move(RIGHT)
      case Guard(RIGHT, p) if collision(matrix, p.right) => guard.move(DOWN)
      case Guard(DOWN, p) if collision(matrix, p.down) => guard.move(LEFT)
      case Guard(LEFT, p) if collision(matrix, p.left) => guard.move(UP)
      case Guard(d, _) => guard.move(d)
    
  @tailrec
  def loop(guard: Guard, matrix: Map[Position, Char]): Map[Position, Char] =
    var next = step(guard, matrix)
    if (next.outside(matrix))
      matrix + (guard.position -> 'X')
    else 
      loop(next, matrix + (guard.position -> 'X'))

  def part1(input: String): Int = 
    val matrix = parse(input)

    val guard = matrix.find:
      case (_, ch) => ch == '^' || ch == '>' || ch == 'v' || ch == '<'
    .map:
      case (p, '^') => Guard(UP, p)
      case (p, '>') => Guard(RIGHT, p)
      case (p, 'v') => Guard(DOWN, p)
      case (p, '<') => Guard(LEFT, p)
    .get

    loop(guard, matrix).count((_, ch) => ch == 'X')

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day6.txt").getLines().mkString("\n")
  println(Day6.part1(input))
  println(Day6.part2(input))

