package day10

import scala.io.Source
import scala.annotation.tailrec

// https://adventofcode.com/2024/day/10
object Day10:

  case class Position(x: Int, y: Int):
    def up = Position(x, y + 1)
    def down = Position(x, y - 1)
    def right = Position(x + 1, y)
    def left = Position(x - 1, y)
    def adjacent = List(up, down, left, right)

  enum Tree:
    case Leaf(value: Position)
    case Node(value: Position, branches: List[Tree])
    def leaves(matrix: Map[Position, Int]): List[Position] = 
      this match
        case Leaf(value) if (matrix(value) == 9) => List(value)
        case Node(_, branches) => branches.flatMap(_.leaves(matrix))
        case _ => Nil
      
  import Tree._

  def parse(input: String): Map[Position, Int] = 
    input.linesIterator.zipWithIndex.flatMap:
      case (line, y) => line.zipWithIndex.map:
        case (ch, x) => Position(x, y) -> (ch.toInt - 48)
    .toMap

  def search(matrix: Map[Position, Int])(start: Position): Tree = 
    val next = start.adjacent.filter(matrix.contains).filter(p => matrix(p) - matrix(start) == 1)

    if (next.isEmpty)
      Leaf(start)
    else
      Node(start, next.map(search(matrix)))

  def part1(input: String): Int = 
    val matrix = parse(input)

    val zeros = matrix.filter:
      case (p, i) => i == 0
    .keySet.toList

    zeros.map(search(matrix)).map(_.leaves(matrix).toSet.size).sum

  def part2(input: String): Int = 
    val matrix = parse(input)

    val zeros = matrix.filter:
      case (p, i) => i == 0
    .keySet.toList

    zeros.map(search(matrix)).map(_.leaves(matrix).size).sum

@main def main: Unit =
  val input = Source.fromFile("input/day10.txt").getLines().mkString("\n")
  println(Day10.part1(input))
  println(Day10.part2(input))

