package day8

import scala.io.Source

// https://adventofcode.com/2024/day/8
object Day8:
  
  case class Position(x: Int, y: Int):

    def antinodes(other: Position): List[Position] = 
      val diffx = this.x - other.x
      val diffy = this.y - other.y

      List(
        Position(this.x + diffx, this.y + diffy),
        Position(other.x - diffx, other.y - diffy),
      )

  def parse(input: String): Map[Position, Char] =
    input.linesIterator.zipWithIndex.flatMap:
      (line, y) => line.zipWithIndex.map:
        (ch, x) => Position(x, y) -> ch
    .toMap

  def part1(input: String): Int = 
    val matrix = parse(input)
    val antennas = matrix.values.toSet.filterNot(_ == '.').map:
      a => matrix.filter((_, ch) => ch == a).keySet.toList
    
    val antinodes = antennas.flatMap:
      _.combinations(2).toList.flatMap:
        case List(a, b) => a.antinodes(b)
    .filter(matrix.contains)
    .toList

    antinodes.size

  def part2(input: String): Int = ???

@main def main: Unit =
  val input = Source.fromFile("input/day8.txt").getLines().mkString("\n")
  println(Day8.part1(input))
  println(Day8.part2(input))

