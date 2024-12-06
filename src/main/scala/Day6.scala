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
    def move: Guard = 
      direction match
        case UP => Guard(direction, position.up)
        case RIGHT => Guard(direction, position.right)
        case DOWN => Guard(direction, position.down)
        case LEFT => Guard(direction, position.left)

    def turn(next: Direction): Guard = Guard(next, position)
    
    def outside(matrix: Map[Position, Char]): Boolean = !matrix.contains(position)

  def parse(input: String): Map[Position, Char] =
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        (ch, x) => Position(x, y) -> ch
    .toMap

  def findGuard(matrix: Map[Position, Char]): Guard =
    matrix.find:
      case (_, ch) => ch == '^'
    .map:
      case (p, '^') => Guard(UP, p)
      case (p, '>') => Guard(RIGHT, p)
      case (p, 'v') => Guard(DOWN, p)
      case (p, '<') => Guard(LEFT, p)
    .get

  def collision(matrix: Map[Position, Char], next: Position): Boolean =
    matrix.get(next) match
      case Some(ch) => ch == '#'
      case None => false
  
  @tailrec
  def step(guard: Guard, matrix: Map[Position, Char]): Guard = 
    guard match
      case Guard(UP, p) if (collision(matrix, p.up)) => step(guard.turn(RIGHT), matrix)
      case Guard(RIGHT, p) if collision(matrix, p.right) => step(guard.turn(DOWN), matrix)
      case Guard(DOWN, p) if collision(matrix, p.down) => step(guard.turn(LEFT), matrix)
      case Guard(LEFT, p) if collision(matrix, p.left) => step(guard.turn(UP), matrix)
      case _ => guard.move

  @tailrec
  def walk(guard: Guard, matrix: Map[Position, Char]): Map[Position, Char] =
    var next = step(guard, matrix)
    if (next.outside(matrix))
      matrix + (guard.position -> 'X')
    else 
      walk(next, matrix + (guard.position -> 'X'))

  @tailrec
  def searchLoop(guard: Guard, matrix: Map[Position, Char], visited: Set[(Position, Position)] = Set.empty): Boolean =
    val next = step(guard, matrix)
    if (next.outside(matrix))
      false
    else if (visited.contains((guard.position -> next.position)))
      true
    else 
      searchLoop(next, matrix, visited + (guard.position -> next.position))

  def part1(input: String): Int = 
    val matrix = parse(input)
    val guard = findGuard(matrix)
    walk(guard, matrix).count((_, ch) => ch == 'X')

  def part2(input: String): Int =
    val matrix = parse(input)
    val guard = findGuard(matrix)
    val visited = walk(guard, matrix).filter((_, ch) => ch == 'X').keySet - guard.position

    visited.filter:
      p => searchLoop(guard, matrix + (p -> '#'))
    .size

@main def main: Unit =
  val input = Source.fromFile("input/day6.txt").getLines().mkString("\n")
  println(Day6.part1(input))
  println(Day6.part2(input))

