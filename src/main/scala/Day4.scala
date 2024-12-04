package day4

import scala.io.Source

// https://adventofcode.com/2024/day/4
object Day4:

  type Move = Position => Position

  case class Position(x: Int, y: Int):
    def right = Position(x + 1, y)
    def left = Position(x - 1, y)
    def up = Position(x, y + 1)
    def down = Position(x, y - 1)
    def rightUp = Position(x + 1, y + 1)
    def rightDown = Position(x + 1, y - 1)
    def leftUp = Position(x - 1, y + 1)
    def leftDown = Position(x - 1, y - 1)

    def apply(move: Move) = move(this)

  def parse(input: String): Map[Position, Char] =
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        (ch, x) => Position(x, y) -> ch
    .toMap

  extension (matrix: Map[Position, Char])
    def filter(target: Char): Set[Position] =
      matrix.filter:
        case (_, ch) => ch == target
      .keySet

  def part1(input: String): Int = 
    val matrix = parse(input)

    val xs = matrix.filter('X')
    val ms = matrix.filter('M')
    val as = matrix.filter('A')
    val ss = matrix.filter('S')

    def search(move: Move): Int =
      xs.filter:
        x => ms.contains(x(move)) && as.contains(x(move andThen move)) && ss.contains(x(move andThen move andThen move))
      .size

    search(_.left) + search(_.right) + search(_.up) + search(_.down) + 
      search(_.rightUp) + search(_.rightDown) + search(_.leftUp) + search(_.leftDown)

  def part2(input: String): Int = 
    val matrix = parse(input)
    
    val as = matrix.filter('A')
    val ms = matrix.filter('M')
    val ss = matrix.filter('S')

    def search(m1: Move, m2: Move, m3: Move, m4: Move): Int =
      as.filter:
        a => ms.contains(a(m1)) && ms.contains(a(m2)) && ss.contains(a(m3)) && ss.contains(a(m4))
      .size

    search(_.leftUp, _.leftDown, _.rightUp, _.rightDown) + 
      search( _.rightUp, _.rightDown, _.leftUp, _.leftDown) + 
      search(_.leftUp, _.rightUp, _.leftDown, _.rightDown) + 
      search(_.leftDown, _.rightDown, _.leftUp, _.rightUp)

@main def main: Unit =
  val input = Source.fromFile("input/day4.txt").getLines().mkString("\n")
  println(Day4.part1(input))
  println(Day4.part2(input))

